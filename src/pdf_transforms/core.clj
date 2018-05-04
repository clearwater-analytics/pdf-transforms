(ns pdf-transforms.core
  "A collection of high level functions for transforming and
  annotating PDF documents"
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as ann]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cls]
            [pdf-transforms.blocks.segments :as bs]
            [pdf-transforms.blocks.core :as bc])
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
       (pe/extract-char-positions pdf-path)
       pd/text-positions->pages-of-tokens))


(defn- pdf->pages-of-lines [pdf-path & [page-bounds]]
  (map utils/create-lines (pages-of-words pdf-path page-bounds)))

(def formats [:tokens :segments :blocks :components])

(defn parse-page [page-of-tokens & [{:keys [format] :or {format :components}}]]
  (let [lvl (.indexOf formats format)]
    (cond->> page-of-tokens
             (> lvl 0) bs/compose-segments
             (> lvl 1) bc/compose-blocks
             (> lvl 2) f/enfeature-blocks
             (> lvl 2) (map cls/add-class)
             (> lvl 2) (cmps/->components page-of-tokens)
             (> lvl 2) (map #(dissoc % :class)))))


(defn- block-transforms [pdf-path {:keys [page-bounds] :as opts}]
  (->> (pages-of-words pdf-path page-bounds)
       (mapcat #(parse-page % opts))))

(defn transform
  "Transforms pages in the range [(first page-bounds) , (last page-bounds))
   of the input pdf (input as a url string or File object) into the desired output format.
   Supported formats include blocks, plain-text, pages-of-lines, and components (default)."
  [pdf-path & [{:keys [page-bounds format] :as opts}]]
  (case (keyword format)
    :plain-text  (pdf->text pdf-path page-bounds)
    :pages-of-lines (pdf->pages-of-lines pdf-path page-bounds)
    (->> (pages-of-words pdf-path page-bounds)
         (mapcat #(parse-page % opts)))))


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
       pe/extract-line-positions
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory})
       dorun))
