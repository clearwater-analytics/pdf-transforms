(ns pdf-transforms.components.columnar
  "Functions for locating vertical columns of whitespace, which usually
  represent semantic boundaries/discontinuities."
  (:require [clojure.set :as sets]
            [pdf-transforms.utilities :as utils]))




;(defn white-between-text
;  "Get a set of all whitespace pixels that are within the bounds on a line"
;  ([line] (white-between-text 0 1000 line))
;  ([l-bound r-bound line]
;   (->> line
;        (map word-to-range)
;        (reduce sets/union)
;        (sets/difference (into #{} (range (max l-bound (let [{:keys [x width]} (first line)] (int (+ x width))))
;                                          (min (inc r-bound) (-> line last :x int))))))))

(defn line->gaps [min-gap-width line]
  (->> line
       (partition 2 1)
       (filter (fn [[{:keys [x width]} {nxt-x :x}]]
                 (>= (- (int nxt-x) (Math/ceil (+ x width))) min-gap-width)))
       (map (fn [[{:keys [x width y height page-number]} {nxt-x :x}]]
              {:x0 (Math/ceil (+ x width)) :x1 (int nxt-x) :y0 (- y height) :y1 y :page-number page-number}))))


(defn overlap-as-boundary [{ax0 :x0 ax1 :x1} {bx0 :x0 bx1 :x1}]
  (let [{:keys [x0 x1] :as V} {:x0 (max ax0 bx0) :x1 (min ax1 bx1)}]
    (when (> x1 x0) V)))

(defn boundary-overlaps
  ":int-groups (white space ranges from this line that do not extend existing boundaries
   :bounds   'can use (filter :overlap bounds) to find the extended columns and terminated ones' "
  [min-width boundaries all-line-gaps]
  (reduce (fn [{:keys [line-gaps bounds] :as X} boundary]
            (if-let [{:keys [gap overlap]} (some (fn [gap]
                                                  (when-let [{:keys [x0 x1]} (overlap-as-boundary gap boundary)]
                                                    (when (> (- x1 x0) min-width) {:overlap (assoc gap :x0 x0 :x1 x1) :gap gap})))
                                   line-gaps)]
              {:line-gaps (remove #(= gap %) line-gaps)
               :bounds     (conj bounds (assoc boundary :overlap overlap))}
              (update X :bounds conj boundary)))
          {:line-gaps all-line-gaps :bounds []} boundaries))

;tests for boundary overlaps
#_(boundary-overlaps 3 [{:x0 100 :x1 150} {:x0 0 :x1 30}] [{:x0 50 :x1 90} {:x0 110 :x1 130 :y1 10} {:x0 160 :x1 190}])

(defn intertext-boundaries [min-width lines]
  (->> lines
       (map (partial line->gaps min-width))
       (reduce (fn [{:keys [active] :as state} gaps]
                 (let [{:keys [line-gaps bounds]} (boundary-overlaps min-width active gaps)]
                   (-> state
                       (update :inactive concat (->> bounds (remove :overlap) (remove :new?)))
                       (assoc :active (concat
                                        (->> bounds
                                             (filter :overlap)
                                             (map (fn [{:keys [overlap x0 x1 y0 y1]}]
                                                    (-> overlap
                                                        (update :y0 (partial min y0))
                                                        (update :y1 (partial max y1))
                                                        (update :x0 (partial max x0))
                                                        (update :x1 (partial min x1))
                                                        (dissoc :overlap :new?)))))
                                        (map #(assoc % :new? true) line-gaps))))))
               {:inactive [] :active []})
       ((fn [{:keys [inactive active]}] (concat inactive (remove :new? active))))))


(defn- word-relative-to-column [{:keys [x1 y0 y1] :as bounds}
                                {:keys [x y]}]
  (cond
    (nil? bounds) :left-of
    (< y y0) :above
    (> y y1) :below
    (> x x1) :right-of
    :else :left-of))


(defn word-to-range
  "Convert a starting value (x) and delta (width) into the corresponding range of numbers"
  [word]
  (into #{} (range (int (:x word)) (int (Math/ceil (+ (:x word) (:width word)))))))

(defn group-consec-integers
  "Turn a range of numbers into a sequence of sequences of numbers, where each sequence of numbers
    contains a continuous sequence of at least min-size elements (e.g. [1 2 3 4 5] but not [1 2 3 5])"
  [min-size integers]
  (->> integers sort
       (utils/partition-when (fn [a b] (not= (inc a) b)))
       (filter #(>= (count %) min-size))))


;;;;;;;  public functions  ;;;;;;;;

(defn find-full-length-columns
  ([min-column-width lines] (find-full-length-columns min-column-width (into #{} (range 0 1000)) lines))
  ([min-column-width base-white-set lines]
   (->> lines
        (map (fn [line] (reduce sets/union (map word-to-range line))))        ;Map a list of word lists to a list of sets of non-whitespace 'pixels'
        (reduce sets/union)                                                   ;Get the set of all non-whitespaces 'pixels' across all word lists on all lines
        (sets/difference base-white-set)
        (into [])                                                             ;Change to a datastructure that has ordering
        (group-consec-integers min-column-width)
        (sort-by #(% 0))                                                      ;Order the boundaries so we go from left of page to right
        (map #(hash-map :x0 (first %)
                        :x1 (last %)
                        :y0 (:y (ffirst lines))
                        :y1 (:y (first (last lines))))))))    ;Return maps representing column boundaries (inclusive)


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











;;;;;;;;;;;;   OLD STUFF   ;;;;;;;;;;;
;;;;  Most of this logic is outdated and was used to split the pdf into sections
;;;;  The intertext-boundaries function above replaces this logic and offers an almost 100% increase
;;;;  in performance (run time) and the resulting boundaries are more in line with how they are used

(defn white-pixels-on-line
  "Get a set of all whitespace pixels that are within the bounds on a line"
  [l-bound r-bound line]
  (->> line
       (map word-to-range)
       (reduce sets/union)
       (sets/difference (into #{} (range l-bound (inc r-bound))))))

(defn white-between-text
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


(defn white-column-overlaps
  "Returns a vector of numbers representing the lowest sequence of at least <min-width> x-coordinates for whitespace pixels
  on <line>, subject to the boundary of <x0> and <x1>."
  [{:keys [x0 x1]} min-width line]
  (->> line
       (white-pixels-on-line x0 x1)
       (group-consec-integers min-width)))


(defn extend-column
  ([column min-width lines] (extend-column column [] min-width lines))
  ([column columns min-width lines]
   (or (when-let [line (first lines)]
         (when-let [overlaps (not-empty (white-column-overlaps column min-width line))]
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
  "Remove boundaries that are merely separating other boundaries"
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
                          (group-consec-integers min-width)
                          (map #(hash-map :x0 (apply min %)
                                          :x1 (apply max %)
                                          :y0 (apply min (map (fn [{:keys [y height]}] (- y height)) line))
                                          :y1 (apply max (map :y line))
                                          :page-number (some :page-number line)))
                          (map #(extend-column % min-width lines))
                          (reduce concat)
                          (filter (fn [{:keys [x0 x1]}] (and (pos? x0) (< x1 1000))))))) ;remove the gutters
      columns)))


(defn find-vertical-boundaries [min-width lines]
  (->> lines
       (find-whitespace-columns min-width)
       remove-redundants))

