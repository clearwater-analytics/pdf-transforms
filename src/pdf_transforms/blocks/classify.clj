(ns pdf-transforms.blocks.classify
  "Functions that use hand-written heuristics to classify blocks according to their associated features."
  (:require [pdf-transforms.utilities :as utils]
            [clojure.string :as s]
            [pdf-transforms.common :as cmn]))

(def page-footer #"(?:[Pp][aAgGeE]{0,3}\s*[.]?\s*\d.*)|[-]?\s*\d{0,3}\s*[-]?")
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(def header-like #".*[a-zA-Z]{2,}.*[^.!?]\s*[\"\'\)]?")


(defn label? [{:keys [tokens]
               {:keys [num-tokens num-blocks-right num-blocks-directly-below
                       gap-right gap-below]} :features}]
  (and
    (pos? num-tokens)
    (< num-tokens 20)
    (or (and (pos? num-blocks-right) (< gap-right 200))
        (and (pos? num-blocks-directly-below) (< gap-below 50)))
    (re-matches header-like (s/join " " (map :text tokens)))))

(defn page-footer? [{{:keys [num-blocks-below
                             itemized-start?]} :features
                     tokens             :tokens}]
  (and (or itemized-start? (re-matches page-footer (s/join " " (map :text tokens))))
       (<= num-blocks-below 2)))

(defn table-footer? [{{:keys [superscript-start?]} :features
                      tokens :tokens}]
  (let [as-text (s/join " " (map :text tokens))]
    (or
      (re-matches utils/dash-line as-text)                  ;____ or ----- line
      superscript-start?
      (re-matches footnote as-text))))                      ;lines starts with what should be a superscript

(defn key? [{{:keys [keyword-start? blocks-right
                     bold-ratio italic-ratio text]} :features :as blk}]
  (and (label? blk) text
       (not (re-matches #".+:\s*\S+.*" text))
       (or (and keyword-start? (seq blocks-right))
           (and (or (== 1 bold-ratio) (== 1 italic-ratio))
                (some-> blocks-right first (#(and (< (:bold-ratio %) 1)
                                                     (< (:italic-ratio %) 1))))))))

(defn key-column? [{{:keys [num-blocks-right keyword-start?
                             num-lines]} :features
                    tokens :tokens}]
  (and (pos? num-blocks-right)
       (> num-lines 1)
       keyword-start?
       (> (->> tokens
               utils/create-lines
               (map (comp (partial s/join " ") (partial map :text)))
               (filter (partial re-matches utils/delimited))
               count) 1)
       (re-matches utils/delimited (:text (last tokens)))))

(defn table-column? [{{:keys [num-blocks-right
                              num-blocks-left num-lines
                              num-tokens num-datapoints]} :features}]
  (if (and num-tokens (pos? num-tokens))
    (and (>= (/ num-datapoints num-tokens) 0.5)
         (pos? (+ num-blocks-right num-blocks-left))
         (< (/ num-tokens num-lines) 6))))


(defn labelish? [{{:keys [num-tokens num-blocks-right num-blocks-directly-below
                          gap-right gap-below num-english-words num-datapoints]} :features}]
  (and
    (and (pos? num-english-words) (> num-english-words num-datapoints))
    (< num-tokens 20)
    (or (and (pos? num-blocks-right) (< gap-right 200))
        (and (pos? num-blocks-directly-below) (< gap-below 50)))))

;this is a hierarchy, which is why it is not an (unordered) map
(def class->rules
  [[:key-column          key-column?]
   [:key                 key?]
   [:data-cell          #(and (table-column? %) (= 1 (get-in % [:features :num-lines])))]
   [:table-column        table-column?]
   [:column-n-header     cmn/data-and-labels?]
   [:paragraph           #(pos? (get-in % [:features :num-sentences]))]
   [:text                (fn [{{:keys [num-english-words num-datapoints]} :features}]
                           (and (pos? num-english-words) (> num-english-words num-datapoints)))]
   [:page-footer         page-footer?]
   [:table-footer        table-footer?]])


;TODO remove me ... later ... this is adjusted for oracle colors
(def ml-class->rules
  [[:key-column          key-column?]
   [:key-column                 key?]
   [:data-cell          #(and (table-column? %) (= 1 (get-in % [:features :num-lines])))]
   [:table-column        table-column?]
   [:table-column     cmn/data-and-labels?]
   [:paragraph           #(pos? (get-in % [:features :num-sentences]))]
   [:label            labelish?]
   [:solo-text-block    (fn [{{:keys [num-english-words num-datapoints gap-right gap-left]} :features}]
                          (and (pos? num-english-words) (> num-english-words num-datapoints)
                               (> gap-right 50) (> gap-left 50)))]
   [:text                (fn [{{:keys [num-english-words num-datapoints]} :features}]
                           (and (pos? num-english-words) (> num-english-words num-datapoints)))]
   [:text-rows       (fn [{{:keys [num-english-words num-datapoints num-lines]} :features}]
                       (and (pos? num-english-words) (> num-english-words num-datapoints)
                            (> num-lines 1)))]
   [:footer        page-footer?]
   [:footer        table-footer?]
   [:data-block    (fn [{{:keys [num-english-words num-datapoints]} :features}]
                          (and (pos? num-datapoints) (<= num-english-words num-datapoints))) ]])


(defn classify [block]
  (keep (fn [[class instance-of-class?]] (when (instance-of-class? block) class)) class->rules))

;convenience function
(defn add-class [block]
  (let [classes (classify block)]
    (assoc block :class (first classes)
                 :classes classes)))

(defn ml-classify [block]
  (keep (fn [[class instance-of-class?]] (when (instance-of-class? block) class)) ml-class->rules))


(defn add-ml-class [block]
  (let [classes (ml-classify block)]
    (assoc block :class (first classes)
                 :classes classes)))

