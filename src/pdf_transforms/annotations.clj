(ns pdf-transforms.annotations
  "Functions for creating a modified copy of a pdf, where colored boxes are
   superimposed on the pdf to delimit blocks/components.
   A box's color corresponds to the type of the associated block/component."
  (:require [clojure.string :as s]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.tokens.pdf-extractor :as ext])
  (:import [org.apache.pdfbox.pdmodel PDDocument PDPageContentStream]
           [java.awt Color]
           (java.net URL)
           (java.io File)
           (org.apache.pdfbox.pdmodel.font PDType1Font)))

(def block-colors {:table-cell Color/PINK
                   :table-column Color/MAGENTA
                   :table Color/MAGENTA
                   :column-n-header Color/MAGENTA
                   :key Color/YELLOW
                   :key-column Color/YELLOW
                   :label Color/RED
                   :labeled Color/CYAN
                   :page-footer Color/GREEN
                   :table-footer Color/GREEN
                   :paragraph Color/BLUE})

(def component-colors {:table  Color/MAGENTA
                       :table-column Color/MAGENTA
                       :keywords  Color/RED
                       :superscript Color/CYAN
                       :text   Color/BLUE})

(def CLEAR-COLOR (new Color 1.0 1.0 1.0 0.0))

(defn obj->color [type class]
  (or (component-colors type) (block-colors class)))

(defn output-file [pdf-url out-dir]
  (let [filename (if (and (string? pdf-url) (re-matches #"file:/.*" pdf-url))
                   (last (s/split pdf-url (re-pattern File/separator)))
                   (str "annotated-" (rand-int 9999) ".pdf"))]
    (if out-dir (File. (File. out-dir) filename) (File. filename))))

(defn- draw-rect [stream {:keys [x y width height]}]
  (.addRect stream x y width height)
  (.stroke stream))

(defn- draw-string [stream {:keys [x y id]}]
  (doto stream
    (.setNonStrokingColor CLEAR-COLOR)
    (.beginText)
    (.setFont (PDType1Font/TIMES_ROMAN) 8.0)
    (.moveTextPositionByAmount x y)
    (.drawString id)
    (.endText)))

(defn- annotate-page [{:keys [^PDDocument document ^Long page-num boxes drawn-ids?]}]
  (let [page (.getPage document page-num)
        top-y (-> page .getCropBox .getUpperRightY)]
    (with-open [stream (PDPageContentStream. document page true true true)]
      (.setLineWidth stream 0.7)
      (dorun (for [{:keys [x0 x1 y0 y1 id ^Color color]} boxes]
               (let [coords {:x      x0
                             :y      (- top-y y1)
                             :id id
                             :width  (- x1 x0)
                             :height (Math/abs (- y1 y0))}]
                 (.setStrokingColor stream (or color Color/GRAY))
                 (when (and drawn-ids? id) (draw-string stream coords))
                 (draw-rect stream coords)))))))


(defn annotate [{:keys [pdf-url output-directory table-columns?
                        superscripts? drawn-ids? out-file]}
                blocks]
  (with-open [is (.openStream (URL. pdf-url))
              doc (ext/preprocess-pdf (PDDocument/load is ""))]
    (let [boxes (cond-> blocks
                        table-columns? (concat (->> blocks
                                                    (filter (fn [{:keys [type]}] (= :table type)))
                                                    (mapcat utils/infer-column-boundaries)))
                        superscripts? (concat (->> blocks
                                                   (map :vals)
                                                   flatten
                                                   (filter :superscript?)
                                                   (map (fn [{:keys [y height x width page-number]}]
                                                          {:x0   x :x1 (+ x width) :y1 y :y0 (- y height)
                                                           :type :superscript :page-number page-number})))))]
      (->> boxes
           (map (fn [{:keys [type class] :as cmp}]
                  (assoc cmp :color (obj->color type class))))
           (group-by :page-number)
           (map (fn [[page-num page-boxes]] (annotate-page {:document doc :page-num (dec page-num)
                                                            :boxes page-boxes :drawn-ids? drawn-ids?})))
           dorun))
    (.save doc (or (when out-file (File. out-file)) (output-file pdf-url output-directory)))
    blocks))
