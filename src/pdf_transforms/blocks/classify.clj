(ns pdf-transforms.blocks.classify
  "Functions that use hand-written heuristics to classify blocks according to their associated features."
  (:require [pdf-transforms.utilities :as utils]
            [clojure.string :as s]
            [pdf-transforms.common :as cmn]))

(def page-footer #"(?:[Pp][aAgGeE]{0,3}\s*[.]?\s*\d.*)|[-]?\s*\d{0,3}\s*[-]?")
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(def header-like #".*[a-zA-Z]{2,}.*[^.!?]\s*[\"\'\)]?")


(defn label? [{:keys [tokens x1 y1]
               {:keys [num-tokens neighbors-right neighbors-below]} :features}]
  (and
    (pos? num-tokens)
    (< num-tokens 20)
    (or (some-> neighbors-right first (#(< (- (:x0 %) x1) 200)))
        (some-> neighbors-below first (#(< (- (:y0 %) y1) 50))))
    (re-matches header-like (s/join " " (map :text tokens)))))

(defn page-footer? [{{:keys [num-components-below
                             itemized-start?]} :features
                     tokens             :tokens}]
  (and (or itemized-start? (re-matches page-footer (s/join " " (map :text tokens))))
       (<= num-components-below 2)))

(defn table-footer? [{{:keys [superscript-start?]} :features
                      tokens :tokens}]
  (let [as-text (s/join " " (map :text tokens))]
    (or
      (re-matches utils/dash-line as-text)                  ;____ or ----- line
      superscript-start?
      (re-matches footnote as-text))))                      ;lines starts with what should be a superscript

(defn key? [{{:keys [keyword-start? neighbors-right
                     bold-ratio italic-ratio]} :features :as blk}]
  (and (label? blk)
       (or (and keyword-start? (seq neighbors-right))
           (and (or (== 1 bold-ratio) (== 1 italic-ratio))
                (some-> neighbors-right first (#(and (< (:bold-ratio %) 1)
                                                     (< (:italic-ratio %) 1))))))))

(defn key-column? [{{:keys [num-components-right keyword-start?
                             num-lines]} :features
                    tokens :tokens}]
  (and (pos? num-components-right)
       (> num-lines 1)
       keyword-start?
       (> (->> tokens
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
   ;[:label               label?] ;TODO probably need to update the table detection logic to account for this
   [:text                (fn [{{:keys [num-english-words num-datapoints]} :features}]
                           (and (pos? num-english-words) (> num-english-words num-datapoints)))]
   [:page-footer         page-footer?]
   [:table-footer        table-footer?]
   [:paragraph           #(pos? (get-in % [:features :num-sentences]))]])


(defn classify [block]
  (keep (fn [[class instance-of-class?]] (when (instance-of-class? block) class)) class->rules))

;convenience function
(defn add-class [block]
  (let [classes (classify block)]
    (assoc block :class (first classes)
                 :classes classes)))

