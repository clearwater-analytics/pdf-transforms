(ns pdf-transforms.blocks.features
  "Functions to determine feature values and to associate blocks with their feature values"
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]
            [clojure.string :as s]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]))

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


;Potential features
;+ Number-of-period-delimited-sentences
;+ number-of-lines
;+ ends-with-.
;+ font-size (relative to neighbors?)
;+ font (cleaned ?)
;+ num-dates
;+ contains-:


;distance between nearest neighbors in each quadrant (normalized)

;blocks area
;blocks numeric density
;blocks all caps density
;blocks period density
;blocks border padding
;TODO will need to adjust what is fed into this system
(def feature-graph
  "{{:keys [x0 x1 y0 y1 content neighbors]}}"
  {
   :relatives            (fnk [x0 x1 y0 y1 {neighbors nil}]
                           (map (partial cmn/relative-to {:x0 x0 :x1 x1 :y0 y0 :y1 y1}) neighbors))
   :lines                (fnk [content] (utils/create-lines content))
   :text-list            (fnk [content] (map :text content))
   :as-test              (fnk [text-list] (s/join " " text-list))
   :num-tokens           (fnk [content] (count content))
   :horizontal-alignment (fnk [x0 x1] (cond
                                        (utils/within-x? 50 x0 LEFT-MARGIN) :left
                                        (utils/within-x? 20 CENTER-PAGE (/ (+ x1 x0) 2)) :center
                                        :else :right))
   :width                (fnk [x0 x1] (int (- x1 x0)))
   :height               (fnk [y0 y1] (int (- y1 y0)))
   :bold-ratio           (fnk [content num-tokens] (/ (count (filter :bold? content)) num-tokens))
   :italic-ratio         (fnk [content num-tokens] (/ (count (filter :italic? content)) num-tokens))
   :all-caps-ratio       (fnk [text-list num-tokens] (/ (count (filter (partial re-matches utils/all-caps) text-list)) num-tokens))
   :ends-with-period?    (fnk [text-list] (boolean (re-matches utils/sentence-ending (last text-list))))
   :num-english-words    (fnk [text-list]
                           (->> text-list
                                (keep (comp
                                        (partial re-matches utils/eng-wordy)
                                        #(clojure.string/replace % utils/punctuation "")))
                                count))
   :num-datapoints       (fnk [text-list]
                           (->> text-list
                                (keep (partial re-matches utils/datapoint))
                                count))
   :uniform-line-width?  (fnk [lines] (uniform-line-width? lines))
   :num-lines            (fnk [lines] (count lines))
   :num-sentences        (fnk [content]
                           (->> content
                                (remove :superscript?)
                                (map :text)
                                (filter (partial re-matches utils/sentence-ending))
                                count))
   :keyword-start?       (fnk [text-list]
                           (->> text-list
                                (take 10)
                                (some (partial re-matches utils/delimited))
                                boolean))
   :itemized-start?      (fnk [content as-text] (boolean (or (:superscript? (first content)) ;line starts with a superscript
                                                             (re-matches footnote as-text))))
   :num-components-above (fnk [relatives] (count (filter :above relatives)))
   :num-components-below (fnk [relatives] (count (filter :below relatives)))
   :num-components-right (fnk [relatives] (count (filter (partial = #{:right}) relatives)))
   :num-components-left  (fnk [relatives] (count (filter (partial = #{:left}) relatives)))})

(def features2 (graph/compile feature-graph))

(defn enfeature-blocks2 [blocks]
  (map #(assoc % :features (features2 (merge % {:neighbors blocks}))) blocks))

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



;classes
; table-column, table-column-header, text, page-footer, table-footer, label, value, data-point