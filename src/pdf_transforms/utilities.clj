(ns pdf-transforms.utilities
  "Utility functions that are used at various points in the transformation processes."
  (:require [clojure.string :as s]))

(def dummy-token {:y 1.0 :width 1.0 :font "Pseudo-text" :id "0_0_0" :font-size 0.0 :page-number 1 :x 1.0 :height 1.0 :text ""})

(def punctuation #"['\")(:,;*-.]+")
(def eng-wordy #"[a-zA-Z]*[aeiou][a-z]*")
(def all-caps #"[A-Z]+")
(def datapoint #".*(?:\d|[A-Z]{2,}).*|[B-Z]|[-]|[A-Z]+[/-][A-Z]+")
(def sentence-ending #".*[\da-zA-Z\)][.!?][\"\'\)]?")
(def header-like #".*[a-zA-Z]{2,}.*[^.!?]\s*[\"\'\)]?")
(def dash-line #"\s*(?:[-]{3,}|[_]{3,})\s*")
;(def delimited #".*(?:[:-]|[.]{2,})")
(def delimited #".*\:")

(defn gap? [{x0 :x w0 :width t0 :text}
            {x1 :x w1 :width t1 :text}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* 2.25 (min (/ w0 (max 1 (count t0)))
                   (/ w1 (max 1 (count t1)))))))

(defn between?
  "Is a between b and c, inclusive of both bounds?"
  [a b c] (and (>= a b) (<= a c)))

(defn label-ish? [{{:keys [num-tokens
                           num-lines
                           bold-ratio]} :features
                   tokens             :tokens}]
  (and
    (pos? num-tokens)
    (< (/ num-tokens num-lines) 5)
    (> bold-ratio 0.75)
    (re-matches header-like (s/join " " (map :text tokens)))))

(defn label-like? [{{:keys [num-tokens
                            num-lines]} :features
                    tokens             :tokens}]
  (and
    (< num-tokens 14)
    (< (/ num-tokens num-lines) 8)
    (re-matches header-like (s/join " " (map :text tokens)))))

(defn expand-bounds [component {:keys [x0 x1 y0 y1]}]
  (-> component
      (update :y0 (partial min y0))
      (update :y1 (partial max y1))
      (update :x0 (partial min x0))
      (update :x1 (partial max x1))))

(defn within-x? [x a b]
  (<= (Math/abs (- a b)) x))

(defn new-line? [{prev-y :y px :x pw :width p-ss? :superscript? ph :height}
                 {y :y x :x ss? :superscript? h :height}]
  (cond
    p-ss? (> (- y h prev-y ph) -1)
    (and ss? (< (- x (+ px pw)) 3)) (not (between? y (- prev-y ph) prev-y))
    :else (> (- y h prev-y) -1)))

(defn asci-line? [{prev-y :y ph :height} {y :y h :height t :text}]
  (and (pos? (- y 1 prev-y))
       (= "_" t)))

(defn create-lines [positional-data]
  (let [positional-data (sort-by :y positional-data)]
    (map (partial sort-by :x)
         (reduce (fn [lines datum]
                   (if (or (new-line? (first (peek lines)) datum)
                           (asci-line? (first (peek lines)) datum)) ;maybe pass the whole group and the next few positional-data
                     (conj lines [datum])
                     (conj (pop lines) (conj (peek lines) datum))))
                 [[(first positional-data)]]
                 (rest positional-data)))))

(defn assoc-id [{:keys [page-number x y] :as word}]
  (if (and x y)
    (assoc word :id (str page-number "_" (int x) "_" (int y)))
    (do (println word) (assoc word :id "0_0_0"))))

(defn partition-when
  "Partitions seq-data by creating a new partition whenever predicate returns true.
  Predicate should expect two arguments, the previous datum and the current datum."
  [predicate seq-data]
  (reduce (fn [partitions datum]
            (if-let [prev-datum (peek (peek partitions))]
              (if (predicate prev-datum datum)
                (conj partitions [datum])
                (conj (pop partitions) (conj (peek partitions) datum)))
              (conj partitions [datum])))
          [] seq-data))

(defn token->ann-format [{:keys [x width y height] :as token}]
  (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y))

(defn add-tabs [tokens]
  (->> tokens
       (partition-when gap?)
       (map (comp
              (partial s/join " ")
              (partial map :text)))
       (s/join "\t")))

(defn tokens->text [tokens]
  (->> tokens
       create-lines
       (map add-tabs)
       (s/join "\n")))

(defn infer-column-boundaries [{vals :vals}]
  (->> vals
       (apply mapv vector)
       (map
         (comp
           #(hash-map :x0 (apply min (map :x %))
                      :y0 (apply min (map (fn [{:keys [y height]}] (- y height)) %))
                      :x1 (apply max (map (fn [{:keys [x width]}] (+ x width)) %))
                      :y1 (apply max (map :y %))
                      :page-number (:page-number (first %))
                      :type :table-column)
           (partial remove #(= "0_0_0" (:id %)))
           flatten))
       (concat (let [headers (->> (get vals 0) flatten (remove #(= "0_0_0" (:id %))))
                     pg (->> headers first :page-number)
                     y1 (->> headers (map :y) (apply max))]
                 [{:x0 (apply min (map :x headers))
                   :x1 (apply max (map (fn [{:keys [x width]}] (+ x width)) headers))
                   :y0 y1 :y1 y1 :page-number pg :type :table-column}]))))
