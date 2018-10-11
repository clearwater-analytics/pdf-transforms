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
           (java.io File)))

(def block-colors {:table-cell Color/PINK
                   :table-column Color/MAGENTA
                   :column-n-header Color/MAGENTA
                   :key Color/YELLOW
                   :key-column Color/YELLOW
                   :label Color/ORANGE
                   :page-footer Color/CYAN
                   :table-footer Color/GRAY
                   :paragraph Color/BLUE})

(def component-colors {:table  Color/MAGENTA
                       :table-column Color/MAGENTA
                       :keywords  Color/RED
                       :superscript Color/CYAN
                       :text   Color/BLUE})

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

(defn- annotate-page [^PDDocument doc ^Long page-num blocks]
  (let [page (.getPage doc page-num)
        top-y (-> page .getCropBox .getUpperRightY)]
    (with-open [stream (PDPageContentStream. doc page true true true)]
      (.setLineWidth stream 0.7)
      (dorun (for [{:keys [x0 x1 y0 y1 ^Color color]} blocks]
               (do (.setStrokingColor stream (or color Color/GREEN))
                   (draw-rect stream {:x      x0
                                      :y      (- top-y y1)
                                      :width  (- x1 x0)
                                      :height (Math/abs (- y1 y0))})))))))

(defn annotate-simple [pdf-url output-directory components]
  (with-open [doc (ext/preprocess-pdf (PDDocument/load (.openStream (URL. pdf-url)) ""))]
    (->> components
         (map (fn [{:keys [type class] :as cmp}]
                (assoc cmp :color (obj->color type class))))
         (group-by :page-number)
         (map (fn [[page-num comps]] (annotate-page doc (dec page-num) comps)))
         dorun)
    (.save doc (output-file pdf-url output-directory))
    components))

;We assume that components are of the form {:keys [x0 x1 y0 y1 page-number]}
(defn annotate-components [pdf-url output-directory components]
  (with-open [is (.openStream (URL. pdf-url))
              doc (ext/preprocess-pdf (PDDocument/load is ""))]
    (let [tbl-cols (->> components
                        (filter #(= :table (:type %)))
                        (mapcat utils/infer-column-boundaries))
          superscripts (->> components
                            (map :vals)
                            flatten
                            (filter :superscript?)
                            (map (fn [{:keys [y height x width page-number]}]
                                   {:x0 x :x1 (+ x width) :y1 y :y0 (- y height)
                                    :type :superscript :page-number page-number})))]
      (->> components
           (concat tbl-cols superscripts)
           (map (fn [{:keys [type class] :as cmp}]
                  (assoc cmp :color (obj->color type class))))
           (group-by :page-number)
           (map (fn [[page-num comps]] (annotate-page doc (dec page-num) comps)))
           dorun))
    (.save doc (output-file pdf-url output-directory))
    components))
