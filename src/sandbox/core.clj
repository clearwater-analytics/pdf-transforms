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
            [pdf-transforms.tokens.graphics :as g]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            ))



;TODO increase acceptable gap size for footnotes, bullets, (VI) xxx


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
   :visual-features (fnk [tokens] (->> tokens utils/create-lines
                                       (col/intertext-boundaries 4)
                                       (map #(assoc % :class :visual-feature :boundary-axis :x))))
   :segments  (fnk [tokens graphics visual-features]
                (bs/compose-segments tokens (concat visual-features graphics)))
   :blocks    (fnk [segments]
                (bc/compose-blocks segments))})

(def parse-page (graph/lazy-compile page-parser))

(defn merge-graphics [pdf-url pages]
  (let [page->graphics (try (->> (pe/extract-graphics pdf-url)
                                 g/explode-graphics
                                 (group-by :page-number))
                            (catch Exception ex (println "Exception during graphics processing: " (.getMessage ex))))]
    (map #(assoc % :graphics (get page->graphics (some :page-number (:text-positions %)) [])) pages)))

(defn build-pages [pdf-url]
  (->> pdf-url
       pe/extract-text-positions
       (map (fn [{:keys [page-number x y] :as tp}]
              (assoc tp :id (str page-number "_" (int x) "_" (int y)))))
       (group-by :page-number)
       (sort-by key)
       (map (comp (partial array-map :text-positions) second))
       (merge-graphics pdf-url)))


(defn annotate-it [pdf-url & [{:keys [out level] :as opts}]]
  (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)}
              (->> pdf-url
                   build-pages
                   (mapcat (comp level parse-page)))))      ;comp level to get a flat seq of all the blocks or segments



;assumes that batch-folder is in the pdf_parsing directory
(defn annotate-batch [batch-folder & [level]]
  (let [base-dir (str u/home-dir "/Documents/pdf_parsing/" batch-folder "/")]
      (->> (str base-dir "raw")
           u/get-pdfs-in-dir
           (map #(do (println "processing: " %)
                     (annotate-it % {:out (str base-dir (name level)) :level level})))
           dorun)))


(defn annotate-features [pdf-url & [out-dir]]
  (let [graphics (->> pdf-url pe/extract-graphics g/explode-graphics)
        features (->> pdf-url
                      build-pages
                      (mapcat (comp :visual-features parse-page)))]
    (a/annotate {:pdf-url pdf-url :output-directory (or out-dir u/annotated-dir)}
                (concat graphics features))))

#_(->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/deutsche_bank.pdf")
       build-pages
       (mapcat (comp :segments parse-page)))


#_(->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/deutsche_bank.pdf")
       (#(annotate-it % {:level :segments}))
       #_(map #(dissoc % :tokens)))


#_(annotate-batch "control_2" :segments)
#_(annotate-batch "blackrock" :segments)
#_(annotate-batch "aig_1" :features)

#_(annotate-features (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/deutsche_bank.pdf"))
