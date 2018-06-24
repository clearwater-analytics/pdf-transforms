(ns pdf-transforms.annotations
  "Functions for creating a modified copy of a pdf, where colored boxes are
   superimposed on the pdf to delimit blocks/components.
   A box's color corresponds to the type of the associated block/component."
  (:require [clojure.string :as s]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.tokens.pdf-extractor :as ext])
  (:import [org.apache.pdfbox.pdmodel PDDocument]
           (java.net URL)
           (java.io File)
           (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotationSquareCircle PDBorderStyleDictionary)
           (org.apache.pdfbox.pdmodel.graphics.color PDColor PDDeviceRGB)))

(def COLORS {:red (float-array [1.0 0.0 0.0])
             :magenta (float-array [1.0 0.0 1.0])
             :pink  (float-array [1.0 0.7 0.7])
             :green (float-array [0.0 1.0 0.0])
             :cyan  (float-array [0.0 1.0 1.0])
             :blue (float-array [0.0 0.0 1.0])
             :yellow (float-array [1.0 1.0 0.0])})

(defn pd-color [color-name] (new PDColor (COLORS color-name) PDDeviceRGB/INSTANCE))

(def block-colors {:table-cell (pd-color :pink)
                   :table-column (pd-color :magenta)
                   :table (pd-color :magenta)
                   :column-n-header (pd-color :magenta)
                   :key (pd-color :yellow)
                   :key-column (pd-color :yellow)
                   :label (pd-color :red)
                   :labeled (pd-color :cyan)
                   :page-footer (pd-color :green)
                   :table-footer (pd-color :green)
                   :paragraph (pd-color :blue)
                   :graphic (pd-color :green)
                   :visual-feature (pd-color :green)})

(def component-colors {:table  (pd-color :magenta)
                       :table-column (pd-color :magenta)
                       :keywords  (pd-color :red)
                       :superscript (pd-color :cyan)
                       :text   (pd-color :blue)})

(def THICK-BORDER (doto (new PDBorderStyleDictionary) (.setWidth (float 1.0))))

(defn obj->color [type class]
  (or (component-colors type) (block-colors class)))

(defn output-file [pdf-url out-dir]
  (let [filename (if (and (string? pdf-url) (re-matches #"file:/.*" pdf-url))
                   (last (s/split pdf-url (re-pattern File/separator)))
                   (str "annotated-" (rand-int 9999) ".pdf"))]
    (if out-dir (let [dir (File. out-dir)]
                   (.mkdirs dir) (File. dir filename))
                (File. filename))))

(defn box->annotation [{:keys [x0 x1 y0 y1 class type]}]
  (let [position (doto (PDRectangle.)
                   (.setLowerLeftX x0) (.setLowerLeftY y0)
                   (.setUpperRightX x1) (.setUpperRightY y1))]
    (doto
      (PDAnnotationSquareCircle. PDAnnotationSquareCircle/SUB_TYPE_SQUARE)
      (.setColor (or (obj->color type class) (pd-color :red)))
      (.setTitlePopup (str "Class: " (or type class)))
      (.setBorderStyle THICK-BORDER)
      (.setRectangle position))))

(defn- add-page-annotations [{:keys [^PDDocument document ^Long page-num boxes]}]
  (let [page (.getPage document page-num)
        top-y (-> page .getCropBox .getUpperRightY)
        page-annotations (.getAnnotations page)]
    (dorun (for [{:keys [y1 y0] :as box} boxes]
             (let [y (- top-y y1)]
               (.add page-annotations
                     (-> box
                         (assoc :y0 y)
                         (assoc :y1 (+ y (max 0.25 (- y1 y0))))
                         box->annotation)))))))

(defn annotate [{:keys [pdf-url output-directory table-columns?
                        superscripts? out-file]}
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
           (group-by :page-number)
           (map (fn [[page-num page-boxes]] (add-page-annotations {:document doc :page-num (dec page-num)
                                                                   :boxes page-boxes})))
           dorun))
    (.save doc (or (when out-file (File. out-file)) (output-file pdf-url output-directory)))
    blocks))
