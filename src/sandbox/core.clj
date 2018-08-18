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
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cl]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.components.core :as cmps]
            [clojure.string :as s]))


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

(defn token->ann-format [{:keys [x width y height] :as token}]
  (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y))


(def page-parser
  {:tokens          (fnk [text-positions] (pd/page->token-stream text-positions))
   :visual-features (fnk [tokens] (->> tokens utils/create-lines
                                       (col/intertext-boundaries 4)
                                       (map #(assoc % :class :visual-feature :boundary-axis :x))))
   :segments        (fnk [tokens graphics visual-features] (bs/compose-segments tokens (concat visual-features graphics)))
   :blocks          (fnk [segments graphics]
                      (->> (bc/compose-blocks segments graphics)
                           f/enfeature-blocks
                           (map cl/add-class)))
   :experimental     (fnk [segments]
                         segments                              ;this is just a filler

                      )
   :components      (fnk [tokens blocks]
                      (cmps/->components tokens blocks))})

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Repl snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (annotate-batch "control_1" :segments)
  (annotate-batch "control_2" :segments)
  (annotate-batch "control_2" :blocks)
  (annotate-batch "blackrock" :segments)
  (annotate-batch "blackrock" :blocks)
  (annotate-batch "aig_1" :features)

  ;annotate tokens example
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/a150c74d7a9d72d4c69ad8a3f5084fcf.pdf")]
    (->> pdf
         build-pages
         (mapcat (comp :tokens parse-page))
         (map (comp #_#(assoc % :class (when (re-matches #"[^\d]*[aeiouyAEIOUY]+[^\d]*" (:text %)) :paragraph)) token->ann-format))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;annotate lines
  (let [pdf (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/edgar.pdf")]
    (->> pdf
         build-pages
         (mapcat (comp :lines parse-page))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;parse segments
  (->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/866c354c846ed29c9d415dd6066aecd8.pdf")
       build-pages
       (mapcat (comp :segments parse-page))
       (take 10)
       )


  #_(->> (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/repeat_tbl_with_groups.pdf")
         (#(annotate-it % {:level :blocks}))
         (map (comp #(dissoc % :tokens) #(assoc % :text (s/join " " (map :text (:tokens %)))))))

  #_(annotate-features (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/S2_trust.pdf"))




  )