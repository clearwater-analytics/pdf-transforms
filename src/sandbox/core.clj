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
            [clojure.set :as sets]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            ))


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

(def levels [:tokens :segments :blocks])                    ;TODO add :components later

;TODO can dissoc old keys (i.e. dissoc segments after blocks are made) later if needed
#_(defn parse-page [page-of-tokens & [{:keys [level] :or {level :blocks}}]]
  (let [lvl (.indexOf levels level)
        token->ann-format (fn [{:keys [x width y height] :as token}]
                            (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y ))
        assc (fn [k fnc x] (assoc x k (fnc x)))]
    (cond->> page-of-tokens
             (zero? lvl) (map token->ann-format)        ;just for the sake of annotating tokens
             (> lvl 0) (assc :segments bs/compose-segments)
             (> lvl 1) (assc :blocks bc/compose-blocks)
             (> lvl 1) f/enfeature-blocks
             (> lvl 1) cls/classify-blocks)))

;token->ann-format (fn [{:keys [x width y height] :as token}]
;                    (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y ))

;white space dividers logic is really slow
(def page-parser
  {:tokens    (fnk [text-positions] (pd/page->token-stream text-positions))
   :whitespace-dividers (fnk [tokens] (->> tokens utils/create-lines (col/find-vertical-boundaries 4))) ;still really slow
   :segments  (fnk [tokens graphics whitespace-dividers]
                (bs/compose-segments tokens graphics whitespace-dividers))
   :blocks    (fnk [segments]
                (bc/compose-blocks segments))})

(def parse-page (graph/lazy-compile page-parser))

(defn build-pages [pdf-url]
  (let [text-pages (->> pdf-url
                        pe/extract-text-positions
                        (map (fn [{:keys [page-number x y] :as tp}]
                               (assoc tp :id (str page-number "_" (int x) "_" (int y)))))
                        (group-by :page-number)
                        (sort-by key)
                        (map (comp (partial array-map :text-positions) second)))]
    (let [page->graphics (group-by :page-number (pe/extract-graphics pdf-url))]
      (map #(assoc % :graphics (page->graphics (some :page-number (:text-positions %)))) text-pages))))

;TODO can worry about ignoring graphics (for performance) later
(defn annotate-it [pdf-url & [{:keys [out level] :as opts}]]
  (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)}
              (->> pdf-url
                   build-pages
                   (mapcat (fn [{:keys [graphics] :as pg}]
                             (concat graphics (level (parse-page pg))))
                           #_(comp level parse-page)
                           ))))      ;comp level to get a flat seq of all the blocks or segments



;assumes that batch-folder is in the pdf_parsing directory
(defn annotate-batch [batch-folder & [level]]
  (let [base-dir (str u/home-dir "/Documents/pdf_parsing/" batch-folder "/")]
      (->> (str base-dir "raw")
           u/get-pdfs-in-dir
           (map #(do (println "processing: " %)
                     (annotate-it % {:out (str base-dir (name level)) :level level})))
           dorun)))






#_(->> (str "866c354c846ed29c9d415dd6066aecd8" ".pdf")
       (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/")
       (#(annotate-it % {:level :blocks}))
       (map #(dissoc % :tokens)))

#_(annotate-batch "control_2" :blocks)
#_(annotate-batch "blackrock" :segments)
#_(annotate-batch "aig_1" :features)
