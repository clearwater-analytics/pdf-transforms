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

(def COLORS {:black (float-array [0.0 0.0 0.0])
             :red (float-array [1.0 0.0 0.0])
             :magenta (float-array [1.0 0.0 1.0])
             :pink  (float-array [1.0 0.7 0.7])
             :light-purple (float-array [0.92 0.77 1.0])
             :green (float-array [0.0 1.0 0.0])
             :dark-green (float-array [0.29 0.55 0.23])
             :cyan  (float-array [0.0 1.0 1.0])
             :light-blue  (float-array [0.09 0.77 1.0])
             :blue (float-array [0.0 0.0 1.0])
             :yellow (float-array [1.0 1.0 0.0])
             :orange (float-array [1.0 0.62 0.31])})

(defn pd-color [color-name]
  (when color-name
    (new PDColor (COLORS color-name) PDDeviceRGB/INSTANCE)))

(def block-colors {:column-n-header :magenta
                   :key             :yellow
                   :labeled         :cyan
                   :page-footer     :green
                   :table-footer    :green
                   :graphic         :green
                   :visual-feature  :cyan})


(def oracle-block-colors {:table           :magenta
                          :key-val         :orange
                          :independent     :blue
                          :fragmented      :cyan})


(def component-colors {:table        :magenta
                       :table-column :magenta
                       :keywords     :red
                       :superscript  :cyan
                       :text         :blue})

(def THICK-BORDER (doto (new PDBorderStyleDictionary) (.setWidth (float 1.0))))

(defn obj->color [type class]
  (or (pd-color (component-colors type)) (pd-color (block-colors class)) (pd-color (oracle-block-colors class))))

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
                   (.setUpperRightX x1) (.setUpperRightY y1))
        color (or (obj->color type class) (pd-color :black))]
    (doto
      (PDAnnotationSquareCircle. PDAnnotationSquareCircle/SUB_TYPE_SQUARE)
      (.setColor color)
      ;(.setInteriorColor color)
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
