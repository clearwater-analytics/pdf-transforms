(ns pdf-transforms.core
  "A collection of high level functions for transforming and
  annotating PDF documents"
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as ann]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.blocks.core :as blks]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cls])
  (:import (org.apache.pdfbox.text PDFTextStripper)))

(defn- pdf->pages-of-lines [pdf-path & [page-bounds]]
  (->> page-bounds
       (pe/extract-char-positions pdf-path)
       pd/->pages-of-words
       (map utils/create-lines)))

(def pdf->text
  (partial pe/process-pdf-document
           (fn [pdf-doc & [[start-page end-page]]]
             (let [stripper (cond-> (PDFTextStripper.)
                                    start-page (doto (.setStartPage start-page))
                                    end-page (doto (.setEndPage end-page)))]
               (.getText stripper pdf-doc)))))



(defn- block-transforms [pdf-path {:keys [page-bounds compose-components?]}]
  (->> (pe/extract-char-positions pdf-path page-bounds)
       pd/->pages-of-words
       (mapcat (fn [page]
                 (cond->> (blks/blocks-on-page page)
                          true f/enfeature-blocks
                          true (map cls/add-class)
                          compose-components? (cmps/->components page)
                          compose-components? (map #(dissoc % :class))
                          true cmn/sort-blocks)))))


(defn transform
  "Transforms pages in the range [(first page-bounds) , (last page-bounds))
   of the input pdf (input as a url string or File object) into the desired output format.
   Supported formats include blocks, plain-text, pages-of-lines, and components (default)."
  [pdf-path & [{:keys [page-bounds format] :as opts}]]
  (case (keyword format)
    :blocks (block-transforms pdf-path opts)
    :plain-text  (pdf->text pdf-path page-bounds)
    :pages-of-lines (pdf->pages-of-lines pdf-path page-bounds)
    (block-transforms pdf-path (assoc opts :compose-components? true))))

(defn annotate-components
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit components.  A box's
   color corresponds to the type of the associated component.  The entire
   pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :components})
       (ann/annotate-components pdf-url output-directory)
       dorun))

(defn annotate-blocks
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit blocks.  A box's
   color corresponds to predicted class of the associated block.
   The entire pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :blocks})
       (ann/annotate-simple pdf-url output-directory)
       dorun))
