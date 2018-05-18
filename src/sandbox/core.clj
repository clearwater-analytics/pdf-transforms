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
            [pdf-transforms.blocks.classify :as cls]))


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



;;;;;;;;;;;; SEGMENTS  ;;;;;;;;;;

(def levels [:tokens :segments :blocks :components])

(defn parse-page [page-of-tokens & [{:keys [level] :or {level :components}}]]
  (let [lvl (.indexOf levels level)]
    (cond->> page-of-tokens
             (> lvl 0) bs/compose-segments
             (> lvl 1) bc/compose-blocks
             (> lvl 1) f/enfeature-blocks
             (> lvl 1) (map cls/add-class))))

(defn annotate-em [pdf-url & [{:keys [out] :as opts}]]
  (->> (pe/extract-char-positions pdf-url)
       pd/text-positions->pages-of-tokens
       (mapcat #(parse-page % opts))
       (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)})))

#_(->> (str "866c354c846ed29c9d415dd6066aecd8" ".pdf")
       (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/")
       (#(annotate-em % {:level :tokens}))
       (map #(dissoc % :tokens)))

#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(do (println "processing: " %)
                   (annotate-em % {:out (str base-dir "blocks") :level :blocks})))
         dorun))
