(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.blocks.core :as bc]
            [pdf-transforms.blocks.segments :as bs]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.core :as core]
            [sandbox.utils :as u]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cls]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.components.columnar :as col]
            [clojure.set :as sets]))


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

;(defn new-segment? [{ax :x :as a} {bx :x :as b}]
;  (or (> ax bx) (horizontal-gap? a b) (utils/new-line? a b)))


;TODO better column detection, and maybe include features on each page instead of just having a vector of segments/tokens
;  {:keys [columns graphics tokens height width]}

;;;;;;;;;;;; SEGMENTS  ;;;;;;;;;;

(def levels [:tokens :segments :blocks :components])

(defn parse-page [page-of-tokens & [{:keys [level] :or {level :components}}]]
  (let [lvl (.indexOf levels level)
        token->ann-format (fn [{:keys [x width y height] :as token}]
                            (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y ))]
    (cond->> page-of-tokens
             (zero? lvl) (map token->ann-format)            ;just for the sake of annotating tokens
             (> lvl 0) bs/compose-segments
             (> lvl 1) bc/compose-blocks
             (> lvl 1) f/enfeature-blocks
             (> lvl 1) (map cls/add-class)
             (> lvl 2) (cmps/->components page-of-tokens)
             (> lvl 2) (map #(dissoc % :class)))))

(defn parse-features [page-of-tokens]
  (->> page-of-tokens
       utils/create-lines
       (col/find-vertical-boundaries 4)))

(defn annotate-it [pdf-url & [{:keys [out level] :as opts}]]
  (let [annotations (cond
                      (= :graphics level) (pe/extract-line-positions pdf-url)
                      (= :features level) (->> (pe/extract-char-positions pdf-url)
                                               pd/text-positions->pages-of-tokens
                                               (mapcat parse-features))
                      :else (->> (pe/extract-char-positions pdf-url)
                                 pd/text-positions->pages-of-tokens
                                 (mapcat #(parse-page % opts))))]
    (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)} annotations)))

;assumes that batch-folder is in the pdf_parsing directory
(defn annotate-batch [batch-folder & [level]]
  (let [base-dir (str u/home-dir "/Documents/pdf_parsing/" batch-folder "/")]
      (->> (str base-dir "raw")
           u/get-pdfs-in-dir
           (map #(do (println "processing: " %)
                     (annotate-it % {:out (str base-dir (name level)) :level level})))
           dorun)))






#_(->> (str "0a0d474f5c51da7b3031cb4cc5d5a1db" ".pdf")
       (str "file:" u/home-dir "/Documents/pdf_parsing/control_1/raw/")
       (#(annotate-it % {:level :blocks}))
       (map #(dissoc % :tokens)))

#_(annotate-batch "control_2" :blocks)
#_(annotate-batch "blackrock" :segments)
#_(annotate-batch "aig_1" :features)
