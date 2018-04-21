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


;TODO better gap logic
; End of sentence
;    bob." hello
;    JON. SALLY
;    today. Co-owner
;    purposes. Any
;    borrowers.  See,

; Footnotes, superscripts, bullets, Itemized lines all need a larger gap threshold

;Mid sentence gaps
; yment related defaults, if mate financial difficulties; (iv) difficulties; (v) substitution of
; Council; former Board member and past President Association of Counties; past President,
;  Missouri-Columbia (B.S., Business



;TODO increase acceptable gap size for footnotes, bullets, (VI) xxx




;;;;;;;;;;;; SEGMENTS  ;;;;;;;;;;

(def segment-decor-graph
  {:text                 (fnk [tokens] (s/join " " (map :text tokens)))
   ;:width-ratio          (fnk [page-bounds x0 x1]
   ;                        (/ (- x1 x0)
   ;                           (- (:x1 page-bounds)
   ;                              (:x0 page-bounds))))
   ;:width-class          (fnk [width-ratio]
   ;                        (cond
   ;                          ;(>= width-ratio 0.95) 4
   ;                          (>= width-ratio 0.75) 3
   ;                          (>= width-ratio 0.5) 2
   ;                          (>= width-ratio 0.25) 1
   ;                          :else 0))
   ;:horizontal-alignment (fnk [x0 x1 page-bounds width-class]
   ;                        (let [{px0 :x0 px1 :x1} page-bounds]
   ;                          (cond
   ;                            (or
   ;                              (utils/within-x? 10 x0 px0)
   ;                              (and (utils/within-x? 50 x0 px0)
   ;                                   (> (- (- x0 px0) (- px1 x1)) 10)
   ;                                   (> width-class 2))) :left
   ;                            (and
   ;                              (> (- px1 x1) 50)
   ;                              (utils/within-x? 10 (- px1 x1) (- x0 px0))) :center
   ;                            (utils/within-x? 15 x1 px1) :right
   ;                            :else :float)))
   :height               (fnk [y0 y1] (- y1 y0))
   :font                 (fnk [tokens] (->> tokens (map (comp b/font-clean :font)) frequencies (apply max-key second) first))
   :font-size            (fnk [tokens] (->> tokens (map :font-size) frequencies (apply max-key second) first))})

(def decorate (graph/compile segment-decor-graph))

(defn decorate-segment [page-boundaries tokens]
  (-> tokens
      cmn/boundaries-of
      (assoc :tokens tokens :page-bounds page-boundaries)
      ((juxt identity decorate))
      ((partial apply merge))))

(defn vertically-near? [{:keys [y1 font font-size height]}
                        {by0 :y0 b-font :font b-font-size :font-size b-height :height}]
  (< (- by0 y1) (* (if (and (= font b-font) (= font-size b-font-size))
                     2.0
                     1.0)
                   (min height b-height))))

;TODO can increase performance by short-circuiting ...
(defn filter-candidates [segment other-segments blocks]
  (:candidates
    (reduce (fn [{:keys [candidates] :as box} seg]
              (if (and (= #{:below} (cmn/relative-to box seg))
                       (vertically-near? (last candidates) seg))
                (let [{cands :candidates :as hypothesis} (-> box (utils/expand-bounds seg) (update :candidates conj seg))]
                  (if (some (fn [seg-x] (and (empty? (cmn/relative-to hypothesis seg-x)) ;within block
                                             (not (some (partial = seg-x) cands))))
                            (concat other-segments blocks))
                    (reduced box)
                    hypothesis))
                box))
            (assoc segment :candidates [segment])
            other-segments)))





(defn group-vertically [segment other-segments other-blocks]
  (let [calc-diff #(- (:y0 (second %)) (:y1 (first %)))
        pairs (->> other-blocks
                   (filter-candidates segment other-segments)
                   (partition 2 1))]
    (case (count pairs)
      0 [segment]
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
  (loop [blocks []
         remaining segments]
    (if-not (seq remaining)
      blocks
      (let [block (group-vertically (first remaining) (rest remaining) blocks)]
        (recur (conj blocks (format-block block)) (remove (into #{} block) remaining))))))

(def levels [:tokens :segments :blocks :components])

(defn parse-page [page-of-tokens & [{:keys [level] :or {level :components}}]]
  (let [lvl (.indexOf levels level)
        page-bounds (cmn/boundaries-of page-of-tokens)]
    (cond->> page-of-tokens
             (> lvl 0) (utils/partition-when b/new-segment?)
             (> lvl 0) (map (partial decorate-segment page-bounds))
             (> lvl 1) build-blocks)))

(defn annotate-em [pdf-url & [{:keys [out] :as opts}]]
  (->> (pe/extract-char-positions pdf-url)
       pd/text-positions->pages-of-tokens
       (mapcat #(parse-page % opts))
       (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)})))

#_(->> {:level :segments}
       (annotate-em (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/transposed_table.pdf"))
       (map #(dissoc % :tokens))
       )

#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_3/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(annotate-em % {:out (str base-dir "segments") :level :segments}))
         dorun))

#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(do (println "processing: " %)
                   (annotate-em % {:out (str base-dir "blocks") :level :blocks})))
         dorun))
