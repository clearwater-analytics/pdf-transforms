(ns pdf-transforms.blocks.features
  "Functions to determine feature values and to associate blocks with their feature values"
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]
            [clojure.string :as s]))

;50 -> 600
(def LEFT-MARGIN 50)
(def RIGHT-MARGIN 600)
(def CENTER-PAGE 325)
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(defn uniform-line-width? [lines]
  (and (> (count lines) 2)
       (->> lines
            (map (comp (fn [{:keys [width x]}] (+ x width)) last))
            butlast
            (partition 2 1)
            (every? (partial apply utils/within-x? 10)))))

;distance between neighbors (normalized)

(defn features [{:keys [x0 x1 y0 y1 content] :as cmpnt} & [other-components]]
  (let [relatives (map (partial cmn/relative-to cmpnt) other-components)
        lines (utils/create-lines content)
        text-list (map :text content)
        as-text (s/join " " text-list)
        num-tokens (count content)]
    (merge {:horizontal-alignment (cond
                                    (utils/within-x? 50 x0 LEFT-MARGIN) :left
                                    (utils/within-x? 20 CENTER-PAGE (/ (+ x1 x0) 2)) :center
                                    :else :right)
            :width                (int (- x1 x0))
            :height               (int (- y1 y0))
            :bold-ratio           (/ (count (filter :bold? content)) num-tokens)
            :italic-ratio         (/ (count (filter :italic? content)) num-tokens)
            :all-caps-ratio       (/ (count (filter (partial re-matches utils/all-caps) text-list)) num-tokens)
            :ends-with-period?    (boolean (re-matches utils/sentence-ending (last text-list)))
            :num-english-words    (->> text-list
                                       (keep (comp
                                               (partial re-matches utils/eng-wordy)
                                               #(clojure.string/replace % utils/punctuation "")))
                                       count)
            :num-datapoints       (->> text-list
                                       (keep (partial re-matches utils/datapoint))
                                       count)
            :uniform-line-width?  (uniform-line-width? lines)
            :num-tokens           num-tokens
            :num-lines            (:sum (reduce (fn [{:keys [sum prev-y]} {y :y}]
                                                  {:sum    (+ sum (if (> (- y prev-y) 4) 1 0))
                                                   :prev-y y})
                                                {:sum 1 :prev-y 1000} (sort-by :y content)))
            :num-sentences        (->> content
                                       (remove :superscript?)
                                       (map :text)
                                       (filter (partial re-matches utils/sentence-ending))
                                       count)
            :keyword-start?       (boolean (some (partial re-matches utils/delimited) (take 10 text-list)))
            :itemized-start?      (boolean (or (:superscript? (first content)) ;line starts with a superscript
                                               (re-matches footnote as-text)))}
           (if other-components
             {:num-components-above (count (filter :above relatives))
              :num-components-below (count (filter :below relatives))
              :num-components-right (count (filter (partial = #{:right}) relatives))
              :num-components-left  (count (filter (partial = #{:left}) relatives))}))))


(defn enfeature-blocks [blocks]
  (map #(assoc % :features (features % blocks)) blocks))

;Potential features
;+ Number-of-period-delimited-sentences
;+ number-of-lines
;+ ends-with-.
;+ font-size (relative to neighbors?)
;+ font (cleaned ?)
;+ num-dates
;+ contains-:


;classes
; table-column, table-column-header, text, page-footer, table-footer, label, value, data-point