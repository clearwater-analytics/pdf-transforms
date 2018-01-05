(ns pdf-transforms.blocks.classify
  "Functions that use hand-written hueristics to classify blocks according to their associated features."
  (:require [pdf-transforms.utilities :as utils]
            [clojure.string :as s]
            [pdf-transforms.common :as cmn]))

(def page-footer #"(?:[Pp][aAgGeE]{0,3}\s*[.]?\s*\d.*)|[-]?\s*\d{0,3}\s*[-]?")
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(def header-like #".*[a-zA-Z]{2,}.*[^.!?]\s*[\"\'\)]?")


(defn label? [{{:keys [num-tokens
                       num-lines]} :features
               content             :content}]
  (and
    (pos? num-tokens)
    (< num-tokens 20)
    ;(< (/ num-tokens num-lines) 8)
    (re-matches header-like (s/join " " (map :text content)))))

(defn page-footer? [{{:keys [num-components-below
                             itemized-start?]} :features
                     content             :content}]
  (and (or itemized-start? (re-matches page-footer (s/join " " (map :text content))))
       (<= num-components-below 2)))

(defn table-footer? [{{:keys [superscript-start?]} :features
                      content :content}]
  (let [as-text (s/join " " (map :text content))]
    (or
      (re-matches utils/dash-line as-text)                  ;____ or ----- line
      superscript-start?
      (re-matches footnote as-text))))                      ;lines starts with what should be a superscript

(defn key? [{{:keys [keyword-start? num-components-right]} :features :as blk}]
  (and (label? blk)
       keyword-start?
       (pos? num-components-right)))

#_(defn data-heavy-sentence? [{{:keys [ends-with-period? num-datapoints
                                    num-lines num-english-words]} :features}]
  (and (not ends-with-period?)
       (> num-datapoints 1)
       (= 1 num-lines)
       (> num-english-words 1)))

(defn paragraph? [{{:keys [ends-with-period? num-tokens
                          num-lines num-english-words]} :features}]
  (and ends-with-period?
       (> num-lines 1)
       (> (/ num-english-words num-tokens) 0.8)))

(defn key-column? [{{:keys [num-components-right keyword-start?
                             num-lines]} :features
                    content :content}]
  (and (pos? num-components-right)
       (> num-lines 1)
       keyword-start?
       (> (->> content
               utils/create-lines
               (map (comp (partial s/join " ") (partial map :text)))
               (filter (partial re-matches utils/delimited))
               count) 1)))

(defn table-column? [{{:keys [num-components-right
                              num-components-left num-lines
                              num-tokens num-datapoints]} :features}]
  (if (and num-tokens (pos? num-tokens))
    (and (>= (/ num-datapoints num-tokens) 0.5)
         (pos? (+ num-components-right num-components-left))
         (< (/ num-tokens num-lines) 6))))


;this is a hierarchy, which is why it is not an (unordered) map
(def class->rules
  [[:key-column          key-column?]
   [:key                 key?]
   [:table-cell          #(and (table-column? %) (= 1 (get-in % [:features :num-lines])))]
   [:table-column        table-column?]
   [:column-n-header     cmn/data-and-labels?]
   [:label               label?]
   [:page-footer         page-footer?]
   [:table-footer        table-footer?]
   [:paragraph           #(pos? (get-in % [:features :num-sentences]))]])


(defn classify [block]
  (some
    (fn [[class instance-of-class?]] (when (instance-of-class? block) class))
    class->rules))

;convenience function
(defn add-class [block]
  (assoc block :class (classify block)))

