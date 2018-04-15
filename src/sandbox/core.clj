(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.blocks.core :as b]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.core :as core]
            [sandbox.utils :as u]
            [clojure.string :as s]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]))


;;;;;;;;;;;; SEGMENTS  ;;;;;;;;;;

(def segment-decor-graph
  {:text      (fnk [tokens] (s/join " " (map :text tokens)))
   ;:mid-x     (fnk [x0 x1] (/ (+ x1 x0) 2.0))
   ;:width     (fnk [x0 x1] (- x1 x0))
   :height    (fnk [y0 y1] (- y1 y0))
   :alignment (fnk [x0 x1] nil)
   :font      (fnk [tokens] (->> tokens (map (comp b/font-clean :font)) frequencies (apply max-key second) first))
   :font-size (fnk [tokens] (->> tokens (map :font-size) frequencies (apply max-key second) first))
   })

(def decorate (graph/compile segment-decor-graph))

(defn decorate-segment [tokens]
  (-> tokens
      cmn/boundaries-of
      (assoc :tokens tokens)
      ((juxt identity decorate))
      ((partial apply merge))))

(defn vertically-near? [{:keys [y1 font font-size height]}
                        {by0 :y0 b-font :font b-font-size :font-size b-height :height}]
  (< (- by0 y1) (* (if (and (= font b-font) (= font-size b-font-size))
                     2.0
                     1.0)
                   (min height b-height))))


(def test-data [{:font-size 9.0,
                 :y1        224.94,
                 :width     68.49008178710938,
                 :font      "BowneKensingtonBold",
                 :class     :generic,
                 :x1        331.9900817871094,
                 :x0        263.5,
                 :y0        220.40399974822998,
                 :height    4.5360002517700195,
                 :text      "TITLE"}
                {:font-size 9.0,
                 :y1        236.44,
                 :width     272.78997802734375,
                 :x1        536.2899780273438,
                 :x0        263.5,
                 :font      "BowneKensingtonRoman",
                 :class     :generic,
                 :y0        232.0975002670288,
                 :height    4.342499732971191,
                 :text      "PARAGRAPH"}
                {:font-size 9.0,
                 :y1        245.94,
                 :x1        513.7898559570312,
                 :x0        263.5,
                 :width     250.28985595703125,
                 :font      "BowneKensingtonRoman",
                 :class     :generic,
                 :y0        241.5975002670288,
                 :height    4.342499732971191,
                 :text      "PARAGRAPH"}
                {:font-size 9.0,
                 :y1        255.44,
                 :width     215.18975830078125,
                 :font      "BowneKensingtonRoman",
                 :class     :generic,
                 :y0        251.0975002670288,
                 :x1        478.68975830078125,
                 :x0        263.5,
                 :height    4.342499732971191,
                 :text      "PARAGRAPH"}
                {:font-size 9.0,
                 :y1        269.94,
                 :width     131.13015747070312,
                 :font      "BowneKensingtonBold",
                 :class     :generic,
                 :y0        265.40399974823,
                 :x1        394.6301574707031,
                 :x0        263.5,
                 :height    4.5360002517700195,
                 :text      "TITLE 2"}])

(defn group-vertically [block other-blocks]
  (let [calc-diff #(- (:y0 (second %)) (:y1 (first %)))
        pairs (->> other-blocks
                   (filter #(= #{:below} (cmn/relative-to block %)))
                   (concat [block])
                   (sort-by :y0)
                   (partition 2 1)
                   (take-while (partial apply vertically-near?)))]
    (case (count pairs)
      0 [block]
      1 (first pairs)
      (:block (reduce (fn [{:keys [last-diff block]} pair]  ;group by magnitude of separation
                        (let [diff (calc-diff pair)]
                          (cond
                            (< (Math/abs (- 1.0 (/ last-diff diff))) 0.3)
                            {:last-diff diff :block (conj block (second pair))}
                            (> diff last-diff)
                            (reduced {:block block})
                            :else (reduced {:block (butlast block)}))))
                      {:last-diff (calc-diff (first pairs))
                       :block     (into [] (first pairs))}
                      (rest pairs))))))

(def format-block
  (comp
    #(select-keys % [:x0 :x1 :y0 :y1 :page-number :tokens])
    (partial reduce (fn [res {:keys [tokens] :as seg}]
                      (-> res
                          (utils/expand-bounds seg)
                          (update :tokens #(concat % tokens)))))))


;TODO can improve 'remove' performance later if necessary
(defn build-blocks [segments]
  (map format-block
       (loop [blocks []
              remaining segments]
         (if-not (seq remaining)
           blocks
           (let [block (group-vertically (first remaining) (rest remaining))]
             (recur (conj blocks block) (remove (into #{} block) remaining)))))))

(def levels [:tokens :segments :blocks :components])

(defn parse-page [page-of-tokens & [{:keys [level] :or {level :components}}]]
  (let [lvl (.indexOf levels level)]
    (cond->> page-of-tokens
             (> lvl 0) (utils/partition-when b/new-segment?)
             (> lvl 0) (map decorate-segment)
             (> lvl 1) build-blocks)))

(defn annotate-em [pdf-url & [{:keys [out] :as opts}]]
  (->> (pe/extract-char-positions pdf-url)
       pd/text-positions->pages-of-tokens
       (mapcat #(parse-page % opts))
       (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)})))

#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(annotate-em % {:out (str base-dir "segments") :lvl :segments}))
         dorun))

#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(do (println "processing: " %)
                   (annotate-em % {:out (str base-dir "blocks") :lvl :blocks})))
         dorun))
