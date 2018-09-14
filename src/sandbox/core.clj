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

            [pdf-transforms.blocks.new-features :as nf]))


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
                     :class (color->class (:color %))) (pe/extract-annotations pdf-url))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Repl snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

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
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/866c354c846ed29c9d415dd6066aecd8.pdf")]
    (->> pdf
         core/build-pages
         (mapcat (comp :blocks core/parse-page))
         (map #(dissoc % :class))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  #_(->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/repeat_tbl_with_groups.pdf")
         (#(annotate-it % {:level :blocks}))
         (map (comp #(dissoc % :tokens) #(assoc % :text (s/join " " (map :text (:tokens %)))))))

  #_(annotate-features (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/headless_col.pdf"))


  (->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/866c354c846ed29c9d415dd6066aecd8.pdf")
       core/build-pages
       (mapcat (comp #_nf/new-enfeature-blocks :blocks core/parse-page))
       (map #(dissoc % :tokens))

       )



  )