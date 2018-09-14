(ns pdf-transforms.common
  "A set of helper functions for composing and assessing blocks and components"
  (:require [pdf-transforms.utilities :as utils]
            [clojure.string :as s]))

(defn in-box? [{:keys [x0 x1 y0 y1]}
               {:keys [y x]}]
  (and (<= y y1) (>= y y0)
       (>= x x0) (<= x x1)))

(defn words-in-box [words box-coords]
  (filter (partial in-box? box-coords) words))

(defn centered? [page-x0 page-x1 {:keys [x0 x1]}]
  (and (> x0 (+ 50 page-x0))                                ;not just left aligned and spanning entire page
       (utils/within-x? 10 (/ (+ x0 x1) 2.0) (/ (+ page-x0 page-x1) 2.0))))

(defn relative-to
  "Returns the position of second box relative to the first box.  Result is a set that contains
  those of the 4 directions are applicable to the relationship. The empty set means that the boxes overlap."
  [{:keys [x0 x1 y0 y1]}
   {sx0 :x0 sx1 :x1 sy0 :y0 sy1 :y1}]
  (cond-> #{}
          (< sy1 y0) (conj :above)
          (> sy0 y1) (conj :below)
          (< sx1 x0) (conj :left)
          (> sx0 x1) (conj :right)))

(defn forgiving-relative-to
  "Returns the position of second box relative to the first box.  Result is a set that contains
  those of the 4 directions are applicable to the relationship.  Padding of 1 is given on each border"
  [{:keys [x0 x1 y0 y1]}
   {sx0 :x0 sx1 :x1 sy0 :y0 sy1 :y1}]
  (cond-> #{}
          (< sy1 (inc y0)) (conj :above)
          (> sy0 (dec y1)) (conj :below)
          (< sx1 (inc x0)) (conj :left)
          (> sx0 (dec x1)) (conj :right)))

(defn within?
  "Is the second box within the first?"
  [{tx0 :x0 tx1 :x1 ty0 :y0 ty1 :y1}
   {:keys [x0 x1 y0 y1]}]
  (and (<= y0 ty1) (>= y1 ty0)
       (<= x0 tx1) (>= x1 tx0)))

(defn overlaps?
  "Is the second box within the first?"
  [{tx0 :x0 tx1 :x1 ty0 :y0 ty1 :y1}
   {:keys [x0 x1 y0 y1]}]
  (when (and tx0 x0)
    (and (<= y0 ty1) (>= y1 ty0)
         (<= x0 tx1) (>= x1 tx0))))

(defn boundaries-of [block]
  (reduce (fn [{:keys [x0 x1 y0 y1] :as st}
               {xw :x ww :width yw :y hw :height ss? :superscript? page :page-number}]
            {:x0          (min x0 xw)
             :x1          (max x1 (+ xw ww))
             :y0          (min y0 (if ss? yw (- yw hw)))
             :y1          (max y1 yw)
             :page-number page})
          {:x0 1000 :x1 0 :y0 1000 :y1 0} block))

(defn sort-blocks [blocks]
  (if-let [blocks (not-empty (sort-by :y0 blocks))]
    (->> (reduce (fn [lines block]
                   (if (:below (relative-to (first (peek lines)) block))
                     (conj lines [block])
                     (conj (pop lines) (conj (peek lines) block))))
                 [[(first blocks)]]
                 (rest blocks))
         (map (partial sort-by :x0))
         (apply concat))
    blocks))

(defn compose-groups [{:keys [terminates? irrelevant? add-to]} raw-units]
  (loop [groups [(add-to nil (first raw-units))]
         processed []
         raw-stream (rest raw-units)]
    (let [group (peek groups)                               ;group is a stream of units
          unit (first raw-stream)]
      (if unit
        (cond
          (terminates? group raw-stream)
          (let [r-stream (concat processed raw-stream)]     ; start new group.
            (recur (conj groups (add-to nil (first r-stream)))
                   []
                   (rest r-stream)))
          (irrelevant? group raw-stream)
          (recur groups
                 (conj processed unit)
                 (rest raw-stream))
          :else
          (recur (conj (pop groups) (add-to group unit))
                 processed
                 (rest raw-stream)))
        (if (empty? processed)
          groups
          (recur (conj groups (add-to nil (first processed)))
                 []
                 (rest processed)))))))

(defn data-column? [{{:keys [num-datapoints num-tokens
                             num-lines]} :features}]
  (if (and num-tokens (pos? num-tokens))
    (and (>= (/ num-datapoints num-tokens) 0.5)
         (< (/ num-tokens num-lines) 6))))

(def token-types
  {:word    #"[a-zA-Z]{2,}.*"
   :numeric #"[$]?\s*(?:\d{1,3}[,])*\d*[.]?\d+\s*[%]?"
   :date    #"\d{1,2}/\d{1,2}/\d{2,4}|[a-zA-Z]{3,}\s*[.]?\s*\d{1,2}\s*[,]?\s*\d{2,4}"})

(defn tokens-type [tokens]
  (let [tkn (s/join " " (map :text tokens))]
    (reduce #(if (re-matches (get token-types %2) tkn)
               (conj %1 %2)
               %1) #{} (keys token-types))))

(defn graphic-line->token [{:keys [x0 x1 y1 page-number]}]
  {:text      "_______" :x x0 :y y1 :height 2.0 :width (- x1 x0) :page-number page-number
   :font-size 10.0 :font "Pseudo-font" :horizontal-bar? true})

(defn ->graphical-lines [lines]
  (->> lines
       (filter (fn [{:keys [y0 y1 x0 x1]}]
                 (and (< (- y1 y0) 2)
                      (> (- x1 x0) 4))))                    ;only care about lines
       (sort-by :y0)
       (utils/partition-when #(> (- (:y0 %2) (:y1 %1)) 2))
       (mapcat (comp
                 (partial map first)
                 (partial utils/partition-when #(> (- (:x0 %2) (:x1 %1)) 4))
                 (partial sort-by :x0)))))

(defn header-index [lines]
  (->> lines
       (map (comp
              (fn [{:keys [word] :as freqs}]
                (/ (or word 0) (max 1 (reduce #(+ %1 (second %2)) 0 freqs))))
              frequencies
              (partial mapcat (comp tokens-type vector))))
       (take-while #(> % 0.5))
       (#(if (pos? (count %)) (dec (count %))))))           ;zero based index

(defn data-and-labels? [{{:keys [num-tokens
                                 num-lines]} :features
                         content             :content}]
  (and
    (< num-tokens 30)
    (< (/ num-tokens num-lines) 8)
    (->> (utils/create-lines content)
         (keep #(let [txt (s/join " " (map :text %))]
                  (cond
                    (re-matches utils/datapoint txt) :data
                    (re-matches utils/header-like txt) :header)))
         ((fn [labels] (and
                         (-> labels first (= :header))
                         (-> labels last (= :data))))))))

(defn table-column? [block]
  (or (data-column? block)
      (data-and-labels? block)))