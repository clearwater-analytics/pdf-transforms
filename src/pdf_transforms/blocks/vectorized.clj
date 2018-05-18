;(ns pdf-transforms.blocks.vectorized
;  (:require [plumbing.core :refer [fnk]]
;            [plumbing.graph :as graph]
;            [pdf-transforms.common :as cmn]
;            [clojure.string :as s]
;            [pdf-transforms.utilities :as utils]))
;
;(def feature-graph
;  {:width             (fnk [x0 x1] (int (- x1 x0)))
;   :height            (fnk [y0 y1] (int (- y1 y0)))
;   :text-list         (fnk [tokens] (map :text tokens))
;   :as-str            (fnk [text-list] (s/join " " text-list))
;   :lines             (fnk [tokens] (utils/create-lines tokens))
;   :token-cnt         (fnk [tokens] (count tokens))
;   :numeric-cnt       (fnk [text-list] (count (filter (partial re-find #"\d") text-list)))
;   :dict-word-cnt     (fnk [text-list]
;                        (->> text-list
;                             (keep (comp
;                                     (partial re-matches utils/eng-wordy)
;                                     #(clojure.string/replace % utils/punctuation "")))
;                             count))
;   :num-lines         (fnk [lines] (count lines))
;   :ends-with-period? (fnk [text-list] (-> text-list last (s/ends-with? ".")))
;
;   }
;  )
;
;
;
;(defn features [{:keys [x0 x1 y0 y1 tokens] :as cmpnt} & [other-components]]
;  (let [relatives (map (partial cmn/relative-to cmpnt) other-components)
;        lines (utils/create-lines tokens)
;        text-list (map :text tokens)
;        as-text (s/join " " text-list)
;        num-tokens (count tokens)]
;    (merge {:horizontal-alignment (cond
;                                    (utils/within-x? 50 x0 LEFT-MARGIN) :left
;                                    (utils/within-x? 20 CENTER-PAGE (/ (+ x1 x0) 2)) :center
;                                    :else :right)
;            :width                (int (- x1 x0))
;            :height               (int (- y1 y0))
;            :bold-ratio           (/ (count (filter :bold? tokens)) num-tokens)
;            :italic-ratio         (/ (count (filter :italic? tokens)) num-tokens)
;            :all-caps-ratio       (/ (count (filter (partial re-matches utils/all-caps) text-list)) num-tokens)
;            :ends-with-period?    (boolean (re-matches utils/sentence-ending (last text-list)))
;            :num-english-words    (->> text-list
;                                       (keep (comp
;                                               (partial re-matches utils/eng-wordy)
;                                               #(clojure.string/replace % utils/punctuation "")))
;                                       count)
;            :num-datapoints       (->> text-list
;                                       (keep (partial re-matches utils/datapoint))
;                                       count)
;            :uniform-line-width?  (uniform-line-width? lines)
;            :num-tokens           num-tokens
;            :num-lines            (:sum (reduce (fn [{:keys [sum prev-y]} {y :y}]
;                                                  {:sum    (+ sum (if (> (- y prev-y) 4) 1 0))
;                                                   :prev-y y})
;                                                {:sum 1 :prev-y 1000} (sort-by :y tokens)))
;            :num-sentences        (->> tokens
;                                       (remove :superscript?)
;                                       (map :text)
;                                       (filter (partial re-matches utils/sentence-ending))
;                                       count)
;            :keyword-start?       (boolean (some (partial re-matches utils/delimited) (take 10 text-list)))
;            :itemized-start?      (boolean (or (:superscript? (first tokens)) ;line starts with a superscript
;                                               (re-matches footnote as-text)))}
;           (if other-components
;             {:num-components-above (count (filter :above relatives))
;              :num-components-below (count (filter :below relatives))
;              :num-components-right (count (filter (partial = #{:right}) relatives))
;              :num-components-left  (count (filter (partial = #{:left}) relatives))}))))

#_(def tmp {:x0 183.94,
          :x1 426.0700378417969,
          :y0 757.9650000953675,
          :y1 762.94,
          :page-number 1,
          :tokens [{:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 16.5,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_183_762",
                    :x 183.94,
                    :height 4.975,
                    :text "The"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 17.699996948242188,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_203_762",
                    :x 203.94,
                    :height 4.975,
                    :text "date"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 8.099990844726562,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_225_762",
                    :x 225.14,
                    :height 4.975,
                    :text "of"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 15.099990844726562,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_236_762",
                    :x 236.73999,
                    :height 4.975,
                    :text "this"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 44.89997863769531,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_255_762",
                    :x 255.33998,
                    :height 4.975,
                    :text "Prospectus"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 50.200042724609375,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_303_762",
                    :x 303.73996,
                    :height 4.975,
                    :text "Supplement"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 6.5,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_357_762",
                    :x 357.44,
                    :height 4.975,
                    :text "is"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 19.10003662109375,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_367_762",
                    :x 367.44,
                    :height 4.975,
                    :text "May"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 12.5,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_390_762",
                    :x 390.04004,
                    :height 4.975,
                    :text "22,"}
                   {:y 762.94,
                    :font-size 10.0,
                    :page-number 1,
                    :width 20.0,
                    :font "CXNKYF+BowneTimesA-Roman",
                    :char-gap 0.0,
                    :id "1_406_762",
                    :x 406.07004,
                    :height 4.975,
                    :text "2007"}]})