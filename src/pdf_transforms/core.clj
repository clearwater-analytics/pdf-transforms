(ns pdf-transforms.core
  "A collection of high level functions for transforming and
  annotating PDF documents"
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as ann]
            [pdf-transforms.blocks.new-features :as f]
            [pdf-transforms.blocks.segments :as bs]
            [pdf-transforms.blocks.core :as bc]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [pdf-transforms.blocks.classify :as cl]
            [pdf-transforms.components.columnar :as col]
            [pdf-transforms.tokens.graphics :as g])
  (:import (org.apache.pdfbox.text PDFTextStripper)))

(def pdf->text
  (partial pe/process-pdf-document
           (fn [pdf-doc & [[start-page end-page]]]
             (let [stripper (cond-> (PDFTextStripper.)
                                    start-page (doto (.setStartPage start-page))
                                    end-page (doto (.setEndPage end-page)))]
               (.getText stripper pdf-doc)))))

(defn pages-of-words [pdf-path & [page-bounds]]
  (->> page-bounds
       (pe/extract-text-positions pdf-path)
       pd/text-positions->pages-of-tokens))


(defn- pdf->pages-of-lines [pdf-path & [page-bounds]]
  (map utils/create-lines (pages-of-words pdf-path page-bounds)))


(def page-parser
  {:tokens          (fnk [text-positions] (pd/page->token-stream text-positions))
   :visual-features (fnk [tokens] (->> tokens utils/create-lines
                                       (col/intertext-boundaries 4)
                                       (map #(assoc % :class :visual-feature :boundary-axis :x))))
   :segments        (fnk [tokens graphics visual-features] (bs/compose-segments tokens (concat visual-features graphics)))
   :blocks          (fnk [segments graphics]
                      (->> (bc/compose-blocks segments graphics)
                           f/ml-enfeature
                           (map cl/add-ml-class)))
   :clusters        (fnk [blocks] (cmps/compose-clusters blocks))
   :components      (fnk [tokens blocks]
                      (cmps/->components tokens blocks))})

(def parse-page (graph/lazy-compile page-parser))

(defn merge-graphics [pdf-url pages]
  (let [page->graphics (try (->> (pe/extract-graphics pdf-url)
                                 g/explode-graphics
                                 (group-by :page-number))
                            (catch Exception ex (println "Exception during graphics processing: " (.getMessage ex))))]
    (map #(assoc % :graphics (get page->graphics (some :page-number (:text-positions %)) [])) pages)))

(defn build-pages
  ([pdf-url] (build-pages nil pdf-url))
  ([page-bounds pdf-url]
   (->> (pe/extract-text-positions pdf-url page-bounds)
        (map (fn [{:keys [page-number x y] :as tp}]
               (assoc tp :id (str page-number "_" (int x) "_" (int y)))))
        (group-by :page-number)
        (sort-by key)
        (map (comp (partial array-map :text-positions) second))
        (merge-graphics pdf-url))))


(defn- block-transforms [pdf-path {:keys [page-bounds] :as opts}]
  (->> (pages-of-words pdf-path page-bounds)
       (mapcat #(parse-page % opts))))

(defn transform
  "Transforms pages in the range [(first page-bounds) , (last page-bounds))
   of the input pdf (input as a url string or File object) into the desired output format.
   Supported formats include tokens, segments, blocks, plain-text, pages-of-lines, and components (default)."
  [pdf-path & [{:keys [page-bounds format] :as opts}]]
  (case (keyword format)
    :plain-text  (pdf->text pdf-path page-bounds)
    :pages-of-lines (pdf->pages-of-lines pdf-path page-bounds)
    (->> pdf-path
         (build-pages page-bounds)
         (mapcat (comp format parse-page)))))


(defn annotate-components
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit components.  A box's
   color corresponds to the type of the associated component.  The entire
   pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :components})
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory
                      :table-columns? true :superscripts? true})
       dorun))


(defn annotate-blocks
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit blocks.  A box's
   color corresponds to predicted class of the associated block.
   The entire pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :blocks})
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory})
       dorun))


(defn annotate-graphics [^String pdf-url & [{:keys [output-directory]}]]
  (->> pdf-url
       pe/extract-graphics
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory})
       dorun))
