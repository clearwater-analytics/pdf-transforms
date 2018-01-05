(ns pdf-transforms.components.columnar
  "Functions for locating vertical columns of whitespace, which usually
  represent semantic boundaries/discontinuities."
  (:require [clojure.set :as sets]))


(defn word-to-range
  "Convert a starting value (x) and delta (width) into the corresponding range of numbers"
  [word]
  (into #{} (range (int (:x word)) (int (Math/ceil (+ (:x word) (:width word)))))))

(defn white-pixels-on-line
  "Get a set of all whitespace pixels that are within the bounds on a line"
  [l-bound r-bound line]
  (->> line
       (map word-to-range)
       (reduce sets/union)
       (sets/difference (into #{} (range l-bound (inc r-bound))))))

(defn white-minus-columns
  "All possible pixels (0 -> 1000) minus those occupied by text on a page"
  [columns]
  (->> columns
       (map #(into #{} (range (:x0 %) (inc (:x1 %)))))
       (reduce sets/union)
       (sets/difference (into #{} (range 0 1000)))))

(defn group-consec-nums
  "Turn a range of numbers into a sequence of sequences of numbers, where each sequence of numbers
  contains a continuous sequence of at least min-size elements (e.g. [1 2 3 4 5] but not [1 2 3 5])"
  [min-size nums]
  (->> nums
       sort
       (reduce (fn [[curr-group & prev-groups :as all-groups] curr-num]
                 (if (or (empty? curr-group) (= (dec curr-num) (peek curr-group)))    ;sequential ?
                   (conj prev-groups (conj curr-group curr-num))    ;add the number to the current sequence
                   (conj all-groups [curr-num])))
               [[]])
       (filter #(>= (count %) min-size))))


(defn white-column-overlaps
  "Returns a vector of numbers representing the lowest sequence of at least <min-width> x-coordinates for whitespace pixels
  on <line>, subject to the boundary of <x0> and <x1>."
  [{:keys [x0 x1]} min-width line]
  (->> line
       (white-pixels-on-line x0 x1)
       (group-consec-nums min-width)))


(defn extend-column
  ([column min-width lines] (extend-column column [] min-width lines))
  ([column columns min-width lines]
    (if-let [line (first lines)]
      (let [overlaps (white-column-overlaps column min-width line)]
        (if (empty? overlaps)
          (conj columns column)
          (reduce concat columns (map #(extend-column (assoc column :x0 (apply min %) :x1 (apply max %) :y1 (:y (first line))) min-width (rest lines)) overlaps))))
      (conj columns column))))


(defn columns-as-pixel-range [columns]
  (->> columns
       (map #(into #{} (range (:x0 %) (inc (:x1 %)))))
       (reduce sets/union)))

(defn- trivial-extension-of?
  "Is the first boundary a trivial extension of the second?"
  [{:keys [x0 x1 y0 y1]} {bx0 :x0 bx1 :x1 by0 :y0 by1 :y1}]
  (and
    (or                 ;adjoining columns?
      (= bx0 (inc x1))
      (= bx1 (dec x0)))
    (and                ;col-a vertically bounded by col-b?
      (>= y0 by0)
      (<= y1 by1))))


;N^2 algorithm, should only be used for one page at a time.  Could improve if need be...
(defn- remove-redundants
  "Remove boundaries that are merely seperating other boundaries"
  [boundaries]
  (->> boundaries
       (map (fn [boundary] (filter #(trivial-extension-of? % boundary) boundaries)))
       (reduce concat)
       (into #{})
       (sets/difference (into #{} boundaries))
       (into [])))


(defn- find-whitespace-columns [min-width all-lines]
  (loop [lines all-lines columns []]
    (if-let [line (first lines)]
      (recur (rest lines)
             (concat columns
                     (->> line
                          (white-pixels-on-line 0 1000)
                          (#(sets/difference % (->> columns
                                                    (filter (fn [{col-y :y1}] (>= col-y (:y (first line))))) ;ignore columns that ended on higher lines
                                                    columns-as-pixel-range)))
                          (group-consec-nums min-width)
                          (map #(hash-map :x0 (apply min %) :x1 (apply max %) :y0 (apply min (map :y line)) :y1 (apply max (map :y line))))
                          (map #(extend-column % min-width lines))
                          (reduce concat)
                          (filter #(and (pos? (:x0 %)) (< (:x1 %) 1000)))))) ;remove the gutters
      columns)))


(defn- word-relative-to-column [{:keys [x1 y0 y1] :as bounds}
                                {:keys [x y]}]
  (cond
    (nil? bounds) :left-of
    (< y y0) :above
    (> y y1) :below
    (> x x1) :right-of
    :else :left-of))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Main Functions                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-full-length-columns
  ([min-column-width lines] (find-full-length-columns min-column-width (into #{} (range 0 1000)) lines))
  ([min-column-width base-white-set lines]
   (->> lines
        (map (fn [line] (reduce sets/union (map word-to-range line))))        ;Map a list of word lists to a list of sets of non-whitespace 'pixels'
        (reduce sets/union)                                                   ;Get the set of all non-whitespaces 'pixels' across all word lists on all lines
        (sets/difference base-white-set)
        (into [])                                                             ;Change to a datastructure that has ordering
        (group-consec-nums min-column-width)
        (sort-by #(% 0))                                                      ;Order the boundaries so we go from left of page to right
        (map #(hash-map :x0 (first %)
                        :x1 (last %)
                        :y0 (:y (first (first lines)))
                        :y1 (:y (first (last lines))))))))    ;Return maps representing column boundaries (inclusive)


(defn find-vertical-boundaries [min-width lines]
  (->> lines
       (find-whitespace-columns min-width)
       remove-redundants))


(defn split-on-column
  ([boundary lines]
   (let [ wrap #(if (vector? (first %)) % [%])
          splits (split-on-column #(if (vector? (first %1)) (conj %1 %2) (vector %1 %2)) boundary lines)]
     {:left-of (wrap (:left-of splits))
      :right-of (wrap (:right-of splits))
      :above (wrap (:above splits))
      :below (wrap (:below splits)) }))
  ([merge-fn boundary lines]
   (->> lines
        (map (fn [line] (group-by #(word-relative-to-column boundary %) line)))
        (filter #(not (empty? %)))
        (apply merge-with merge-fn))))
