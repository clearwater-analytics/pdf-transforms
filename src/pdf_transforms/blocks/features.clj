(ns pdf-transforms.blocks.features
  "Functions to determine feature values and to associate blocks with their feature values"
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]
            [clojure.string :as s]))

(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")
(def word-like #"[^\d]*[aeiouyAEIOUY]+[^\d]*")

(defn uniform-line-width? [lines]
  (and (> (count lines) 2)
       (->> lines
            (map (comp (fn [{:keys [width x]}] (+ x width)) last))
            butlast
            (partition 2 1)
            (every? (partial apply utils/within-x? 10)))))

(defn intra-features [{page-x0 :x0 page-x1 :x1} {:keys [x0 x1 y0 y1 tokens] :as blk}]
  (let [lines (utils/create-lines tokens)
        text-list (map :text tokens)
        as-text (s/join " " text-list)
        num-tokens (count tokens)]
    {:horizontal-alignment (cond
                             (cmn/centered? page-x0 page-x1 blk) :center
                             (utils/within-x? 25 x0 page-x0) :left
                             :else :floating)
     :text                 as-text
     :width                (int (- x1 x0))
     :height               (int (- y1 y0))
     :bold-ratio           (/ (count (filter :bold? tokens)) num-tokens)
     :italic-ratio         (/ (count (filter :italic? tokens)) num-tokens)
     :all-caps-ratio       (/ (count (filter (partial re-matches utils/all-caps) text-list)) num-tokens)
     :ends-with-period?    (boolean (re-matches utils/sentence-ending (last text-list)))
     :num-english-words    (->> text-list
                                (filter (partial re-matches word-like))
                                count)
     :num-datapoints       (->> text-list (keep (partial re-find #"\d")) count)
     :uniform-line-width?  (uniform-line-width? lines)
     :num-tokens           num-tokens
     :num-lines            (:sum (reduce (fn [{:keys [sum prev-y]} {y :y}]
                                           {:sum    (+ sum (if (> (- y prev-y) 4) 1 0))
                                            :prev-y y})
                                         {:sum 1 :prev-y 1000} (sort-by :y tokens)))
     :num-sentences        (->> tokens
                                (remove :superscript?)
                                (map :text)
                                (filter (partial re-matches utils/sentence-ending))
                                count)
     :keyword-start?       (boolean (some (partial re-matches utils/delimited) (take 10 text-list)))
     :itemized-start?      (boolean (or (:superscript? (first tokens)) ;line starts with a superscript
                                        (re-matches footnote as-text)))}))

(defn inter-features [other-components {:keys [x0 x1 y0 y1] :as cmpnt}]
  (let [relatives (map #(vector % (cmn/relative-to cmpnt %)) other-components)
        filter-to (fn [where sort-fn]
                    (some->> relatives (filter (comp #(= #{where} %) second))
                             seq (map first) (sort-by sort-fn) (take 2) (map (fn [{:keys [features] :as cmp}]
                                                                               (merge features (select-keys cmp [:x0 :x1 :y0 :y1]))))))]
    {:num-components-above (count (filter (comp :above second) relatives))
     :neighbors-above      (filter-to :above #(- y0 (:y1 %)))
     :num-components-below (count (filter (comp :below second) relatives))
     :neighbors-below      (filter-to :below #(- (:y0 %) y1))
     :num-components-right (count (filter (comp #(= % #{:right}) second) relatives))
     :neighbors-right      (filter-to :right #(- (:x0 %) x1))
     :num-components-left  (count (filter (comp #(= % #{:left}) second) relatives))
     :neighbors-left       (filter-to :left #(- x0 (:x1 %)))}))


(defn enfeature-blocks [blocks]
  (let [page-dimensions {:x0 (apply min (map :x0 blocks))
                         :x1 (apply max (map :x1 blocks))}
        featured (map #(assoc % :features (intra-features page-dimensions %)) blocks)]
    (map (fn [cmpnt]
           (update cmpnt :features (partial merge (inter-features featured cmpnt)))) featured)))

;classes
; table-column, table-column-header, text, page-footer, table-footer, label, value, data-point