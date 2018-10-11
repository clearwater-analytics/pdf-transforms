(ns pdf-transforms.components.parse
  "Functions for parsing a sequence of lines (a seq. of tokens) into
  a table structure."
  (:require [clojure.string :as s]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.components.columnar :as cols]
            [pdf-transforms.common :as cmn]))

(defn grouping-line? [tokens]
  (let [sorted (sort-by :x tokens)]
    (and (< (count tokens) 8)
         (re-matches utils/header-like (s/join " " (map :text sorted)))
         (-> (reduce (fn [[sum prev] tkn] [(if (utils/gap? prev tkn) (inc sum) sum) tkn])
                     [0 (first sorted)] (rest sorted))
             first
             zero?))))

;assumes column-boundaries are sorted by :x0
(defn index-of [column-boundaries {:keys [x x0]}]
  (let [pred (if x
               #(> x (:x1 %))
               #(> x0 (:x0 %)))]
    (count (take-while pred column-boundaries))))

(defn row-to-vec [column-boundaries row]
  (let [result-vec (vec (repeat (inc (count column-boundaries)) [utils/dummy-token]))]
    (->> row
         (group-by #(index-of column-boundaries %))
         (map #(vector (first %) (second %)))
         (reduce #(assoc %1 (first %2) (second %2)) result-vec))))

(defn conj-words [& words]
  (if (= 1 (count words))
    (first words)
    (let [first-word (first words)
          {:keys [x width]} (last words)
          new-width (- (+ x width) (:x first-word))
          new-text (s/trim (s/join " " (map :text words)))]
      (assoc first-word :text new-text :width new-width))))

(defn split-word [{:keys [text width x] :as word}]
  (if-let [matches (re-matches #"[^.]+(\.{2,})[^.]+" text)]
    (let [[txt1 txt2] (s/split text #"\.{2,}")
          avg-char-width (/ width (- (count text) (/ (count (second matches)) 2)))    ;a dot counts as half a character ... sounds legitimate
          w2 (* avg-char-width (count txt2))]
      [(assoc word :text txt1 :width (* avg-char-width (count txt1)))
       (assoc word :text txt2 :width w2 :x (- (+ x width) w2))])
    word))

(defn build-cells [words]
  (reduce (fn [stream {c-x :x c-text :text ss? :superscript? :as curr-datum}]
            (if-let [last-datum (peek stream)]
              (let [{:keys [x width text f-size]} last-datum
                    x1 (+ x width)
                    gap-width (- c-x x1)]
                (cond
                  (or (re-matches #"\s*[$]\s*" text)        ;should be same cell
                      (re-matches #"[.]{2,}|[%]" c-text)
                      ss?
                      (<= gap-width (* 0.417 (min 12 f-size))))
                  (conj (pop stream) (conj-words last-datum curr-datum))
                  :default (conj stream curr-datum)))
              (conj stream curr-datum)))
          [] words))

(defn column-bounds [lines]
  (->> lines
       (remove grouping-line?)
       (map build-cells)
       (cols/find-full-length-columns 1)
       rest       ;first column is left margin to first bit of text
       butlast))

(defn join-headers-on-line [words]
  (reduce (fn [stream {c-x :x ss? :superscript? :as curr-datum}]
            (if-let [last-data (peek stream)]
              (let [{:keys [x width f-size]} (peek last-data)
                    x1 (+ x width)
                    gap-width (- c-x x1)]
                (if (or ss? (<= gap-width (* 0.417 (min 12 f-size))))
                  (conj (pop stream) (conj last-data curr-datum))
                  (conj stream [curr-datum])))
              (conj stream [curr-datum])))
          [[(first words)]] (rest words)))

(defn distribute-multicol [cells]
  (let [sorted (sort-by :y0 cells)
        [polys monos] (split-with #(empty? (clojure.set/intersection #{:right :left} (into #{} (mapcat (partial cmn/relative-to %) cells)))) sorted)
        new-cells (loop [remaining monos
                         combined []]
                    (if-let [curr-cell (first remaining)]
                      (let [same-cell? #(or (= #{} (cmn/relative-to curr-cell %))
                                            (= #{:below} (cmn/relative-to curr-cell %)))]
                        (recur (remove same-cell? (rest remaining)) (conj combined (mapcat :content (filter same-cell? remaining)))))
                      combined))]
    (map #(merge {:content (concat (mapcat :content polys) %)} (cmn/boundaries-of %)) new-cells)))


(defn adjust-multicol [header-lines]
  (let [cells (map (fn [cell]
                     (merge {:content cell} (cmn/boundaries-of cell)))
                   (mapcat join-headers-on-line header-lines))
        combined-cells (loop [remaining (sort-by :y0 cells)
                              combined []]
                         (if-let [curr-cell (first remaining)]
                           (let [same-cell? #(or (= #{} (cmn/relative-to curr-cell %))
                                                 (= #{:below} (cmn/relative-to curr-cell %)))]
                             (recur (remove same-cell? (rest remaining)) (conj combined (filter same-cell? remaining))))
                           combined))]
    (->> combined-cells
         (map (comp
                #(if (> (count %) 1)
                  (distribute-multicol (flatten %))
                  (let [content (mapcat :content (first %))]
                    (merge {:content content} (cmn/boundaries-of content))))
                (partial utils/partition-when #(< (- (:y0 %2) (:y0 %1)) 4))
                (partial sort-by :y0)))
         flatten)))

(def foot-note-symbol #"\(?.{1,2}\)?")

(defn merge-modifiers [tbl]
  (let [transposed (apply mapv vector tbl)
        labeled-columns (->> transposed
                       (map (fn [column]
                              {:column    column
                               :modifier? (and
                                            (= "0_0_0" (-> column (get 0) first :id)) ;no header
                                            (or
                                              (every? identity (map (comp
                                                                      #(or (= "" %) (re-matches foot-note-symbol %))
                                                                      (partial s/join " ")
                                                                      (partial map :text)) column))
                                              (> (/ (count (filter #(= "0_0_0" (:id (first %))) column))
                                                    (count column)) 0.5)))})))] ;sparse
    (->> labeled-columns
         (reduce (fn [columns {:keys [column modifier?]}]
                   (if (and modifier? (not-empty columns))
                     (conj (pop columns) (mapv concat (peek columns) column))
                     (conj columns column)))
                 [])
         (apply mapv vector))))


(defn primitive-parse [lines]
  (let [header-index (cmn/header-index lines)
        [header-lines body-lines] (split-at (inc (or header-index 0)) lines)
        body-lines (map (comp flatten (partial map split-word)) body-lines)
        bounds (column-bounds body-lines)]
    (->> body-lines
         (apply conj [] (adjust-multicol header-lines))
         (mapv #(row-to-vec bounds %))
         (#(update % 0 (comp
                         (partial map (fn [x] (if (nil? x) [utils/dummy-token] x)))
                         (partial mapcat (partial map :content))))))))

(defn parse-table [lines]
  (-> lines
      primitive-parse
      merge-modifiers))
