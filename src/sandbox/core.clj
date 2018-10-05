(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.blocks.core :as bc]
            [pdf-transforms.blocks.segments :as bs]
            [pdf-transforms.annotations :as a]
            [sandbox.utils :as u]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.components.columnar :as col]
            [pdf-transforms.tokens.graphics :as g]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cl]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.core :as core]
            [clojure.string :as s]

            [pdf-transforms.blocks.new-features :as nf]
            [sandbox.image-seg :as is]))


;;;;;;;;;;;;;    COMPONENTS    ;;;;;;;;;;;;;;;

;each method can take in {:keys [blocks components]} and return the same data structure

(defn labelish? [all-segments {:keys [delimiter-ending? class text x1 tokens] :as segment}]
  (or
    (let [[label & data] (s/split text #":")]
      (and
        (seq data)
        (re-matches #"[^\d]+" label)
        (seq (take-while #(or (:bold %) (:italic %)) tokens))))
    (and delimiter-ending? (re-find #"[a-zA-Z]" text) (some #(= #{:right} (cmn/relative-to segment %)) all-segments))
    (and
      (= class :text)
      (->> all-segments
           (filter (fn [{:keys [x0 ellipsis?] bclass :class btokens :tokens :as seg}]                 ;something close by on the right and vertically aligned
                     (and (= #{:right} (cmn/relative-to segment seg)) ;to the right
                          (< (- x0 x1) 200)          ;close by
                          (or
                            (= :data bclass) ;data point on the right
                            ellipsis?
                            (and
                              (or
                                (every? :bold tokens)
                                (every? :italic tokens))
                              (not (or
                                     (every? :bold btokens)
                                     (every? :italic btokens))))))))
           seq))))



;;;; Parsin' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn annotate-it [pdf-url & [{:keys [out level] :as opts}]]
  (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)}
              (->> pdf-url
                   core/build-pages
                   (mapcat (comp level core/parse-page)))))      ;comp level to get a flat seq of all the blocks or segments



;assumes that batch-folder is in the pdf_parsing directory
(defn annotate-batch [batch-folder & [level oracle?]]
  (let [base-dir (str u/home-dir "/Documents/pdf_parsing/" batch-folder "/")]
      (->> (str base-dir "raw")
           u/get-pdfs-in-dir
           (map #(do (println "processing: " %)
                     (annotate-it % {:out (str base-dir (if oracle? "oracle" (name level))) :level level})))
           dorun)))


(defn annotate-features [pdf-url & [out-dir]]
  (let [graphics (->> pdf-url pe/extract-graphics g/explode-graphics)
        features (->> pdf-url
                      (core/build-pages [0 100])
                      (mapcat (comp :visual-features core/parse-page)))]
    (a/annotate {:pdf-url pdf-url :output-directory (or out-dir u/annotated-dir)}
                (concat graphics features))))


(defn parse-oracle-doc [pdf-url]
  (let [cname->class (zipmap (vals a/oracle-block-colors) (keys a/oracle-block-colors))
        color->class (zipmap (map vec (vals a/COLORS)) (map cname->class (keys a/COLORS)))]
      (map #(assoc % :id (nf/block-id %)
                     :page-number (:page-number %)
                     :class (color->class (:color %))) (pe/extract-annotations pdf-url))))


(defn manual-box-label->block-labels [pdf-url]
  (let [manual-boxes (parse-oracle-doc pdf-url)
        blocks (->> pdf-url core/build-pages (mapcat (comp :blocks core/parse-page)))]
    (mapcat (fn [{:keys [page-number class] :as x}] (->> blocks
                                                         (filter #(and (= page-number (:page-number %)) (cmn/within? x %)))
                                                         (map #(assoc {} :class class
                                                                         :text (get-in % [:features :text])
                                                                         :filename (last (s/split pdf-url #"/"))
                                                                         :id (nf/block-id %))))) manual-boxes)))


(defn blocks-as-feature-vectors [pdf-url]
  (->> pdf-url
       core/build-pages
       (mapcat (comp (partial map #(merge (select-keys % [:x0 :x1 :y0 :y1 :page-number])
                                          (nf/ml-vectorize %)))
                     :blocks core/parse-page))))

(defn block-truth-data [raw-pdf-url oracle-pdf-url]
  (->> (blocks-as-feature-vectors raw-pdf-url)
       (concat (manual-box-label->block-labels oracle-pdf-url))
       (filter :id)
       (group-by :id)
       (map (comp (partial apply merge) second))))



;random forests ....
;TODO assign to multiple categories
(defn blocks-as-classified-features [pdf-url]
  (->> pdf-url
       core/build-pages
       (mapcat (comp (partial map #(merge (select-keys % [:x0 :x1 :y0 :y1 :page-number :class])
                                          {:filename (last (s/split pdf-url #"/"))}
                                          (nf/rand-forest-features %)))
                     :blocks core/parse-page))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Repl snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/54811A4J9.pdf")
          chunk-dimension 5]
      (->> pdf
           core/build-pages
           (mapcat (comp (partial is/segments->chunks chunk-dimension) :segments core/parse-page))
           (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
           dorun))


  ;block-oracle-2
  (spit (str u/home-dir "/blocks.edn") (pr-str (concat (map blocks-as-classified-features (u/get-pdfs-in-dir (str u/home-dir "/Documents/pdf_parsing/control_2/blocks"))))))


  ;build oracle data set
  (let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")
        oracle-pdfs (u/get-pdfs-in-dir (str base-dir "oracle_9_17_2018"))
        raw-pdfs (u/get-pdfs-in-dir (str base-dir "raw"))]
    (map block-truth-data (sort raw-pdfs) (sort oracle-pdfs)))



  (annotate-batch "control_1" :tokens)
  (annotate-batch "control_1" :segments)


  (annotate-batch "control_2" :segments)
  (annotate-batch "control_2" :blocks)
  (annotate-batch "control_2" :components)


  (annotate-batch "blackrock" :segments)
  (annotate-batch "blackrock" :blocks)
  (annotate-batch "aig_1" :features)

  ;annotate tokens example
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/a150c74d7a9d72d4c69ad8a3f5084fcf.pdf")]
    (->> (core/transform pdf {:format :tokens})
         (map (comp #_#(assoc % :class (when (re-matches #"[^\d]*[aeiouyAEIOUY]+[^\d]*" (:text %)) :paragraph)) utils/token->ann-format))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;annotate lines
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/edgar.pdf")]
    (->> pdf
         core/build-pages
         (mapcat (comp :lines core/parse-page))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;parse segments
  (->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/transposed_table.pdf")
       core/build-pages
       (mapcat (comp :blocks core/parse-page))

       )

  ;annotate single doc
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/54811A4J9.pdf")]
    (->> pdf
         core/build-pages
         (mapcat (comp :segments core/parse-page))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  #_(->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/repeat_tbl_with_groups.pdf")
         (#(annotate-it % {:level :blocks}))
         (map (comp #(dissoc % :tokens) #(assoc % :text (s/join " " (map :text (:tokens %)))))))

  #_(annotate-features (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/headless_col.pdf"))


  (a/annotate {:pdf-url (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/54811A4J9.pdf") :output-directory u/annotated-dir}
              (let [step 5]
                (for [x (range 0 600 step)
                      y (range 0 600 step)]
                  {:page-number 1 :x0 x :x1 (+ x step) :y0 y :y1 (+ y step)}
                  ))
              )


  (->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/transposed_table.pdf")
       core/build-pages
       (map #(dissoc % :text-positions))
       #_(mapcat (comp :blocks core/parse-page))
       #_(map (comp #(update % :feature-vec frequencies) nf/ml-vectorize))

       )




  ;TODO maybe try blocks vs segments for image classification





  )