(ns pdf-transforms.tokens.pdf-extractor
  "A collection of low level functions that interop with pdfbox to
  extract the raw data (e.g. text-positions, drawn lines, and bookmarks) from a pdf"
  (:require [pdf-transforms.tokens.char-normalize :as char-norm]
            [clojure.java.io :as io])
  (:import [org.apache.pdfbox.text PDFTextStripper TextPosition]
           (org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline PDOutlineItem)
           (org.apache.pdfbox.pdmodel.interactive.action PDActionGoTo)
           (org.apache.pdfbox.contentstream PDFGraphicsStreamEngine)
           (java.awt.geom Point2D Point2D$Double)
           (org.apache.pdfbox.io RandomAccessBufferedFileInputStream)
           (org.apache.pdfbox.pdfparser PDFParser)
           (org.apache.pdfbox.pdmodel PDPage)
           (org.apache.pdfbox.pdmodel.font PDFont PDFontDescriptor)
           (org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotation)
           (org.apache.pdfbox.pdmodel.common PDRectangle)))


(definterface TextStripperData
  (getData []))

(def italic #"[iI][tT][aA][lL][iI][cC]")
(def bold #"[bB][oO][lL][dD]")

(defn in-memory-text-stripper []
  (let [position-data (atom [])]
    (proxy [PDFTextStripper TextStripperData] []
      (processTextPosition [^TextPosition text]
        (do (swap! position-data #(conj %
                                        (let [^PDFont font (.getFont text)
                                              font-name (if font (.getName font))
                                              ^PDFontDescriptor fdesc (if font (.getFontDescriptor font))]
                                          (merge (if (or (some-> fdesc (.isItalic))
                                                         (re-find italic (or font-name "")))
                                                   {:italic? true})
                                                 (if (or (some-> fdesc (.isForceBold))
                                                         (some-> fdesc (.getFontWeight) (>= 700))
                                                         (re-find bold (or font-name "")))
                                                   {:bold? true})
                                                 {:text        (char-norm/replace-unicode-chars (.getUnicode text))
                                                  :x           (.getXDirAdj text)
                                                  :y           (some-> (.getYDirAdj text) (* 100) float Math/round (/ 100.0))
                                                  :page-number (proxy-super getCurrentPageNo) ;expensive, could get 35 % speed increase by doing this some other way ...
                                                  :font-size   (.getFontSizeInPt text)
                                                  :height      (.getFontSizeInPt text)
                                                  :width       (.getWidthDirAdj text)
                                                  :font        font-name}))))
            (proxy-super processTextPosition text)))
      (getData ([] @position-data)))))

(defn standardize [bounds top-y ay by cy]
  (assoc bounds :y0 (- top-y (max ay by cy))
                :y1 (- top-y (min ay by cy))))

(defn in-memory-line-stripper [^PDPage page]
  (let [line-cutoff 10
        position-data (atom [])
        current-position (atom [0 0])
        top-y (-> page .getCropBox .getUpperRightY)]
    (proxy [PDFGraphicsStreamEngine TextStripperData] [page]
      (appendRectangle [p0 p1 p2 p3]
        (when (> (Point2D/distance (.getX p0) (.getY p0) (.getX p2) (.getY p2)) line-cutoff)
          (swap! position-data #(conj % (standardize {:x0 (.getX p0) :x1 (.getX p2)} top-y (.getY p0) (.getY p1) (.getY p2))))))
      (clip [winding-rule])
      (closePath [])
      (curveTo [x1 y1 x2 y2 x3 y3])
      (drawImage [image])
      (endPath [])
      (fillAndStrokePath [winding-rule])
      (fillPath [winding-rule])
      (getCurrentPoint []
        (Point2D$Double. (first @current-position) (second @current-position)))
      (lineTo [x y]
        (when (> (Point2D/distance (first @current-position) (second @current-position) x y) line-cutoff)
          (swap! position-data #(conj % {:x0 (first @current-position) :y0 (- top-y (second @current-position)) :x1 x :y1 (- top-y y)})))
        (reset! current-position [x y]))
      (moveTo [x y]
        (reset! current-position [x y]))
      (strokePath [])
      (getData ([] @position-data)))))

;; Private Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn preprocess-pdf [doc]
  (do
    (.setAllSecurityToBeRemoved doc true)                   ;decrypt the document
    (let [acro-form (-> doc .getDocumentCatalog .getAcroForm)]
      (if (not (nil? acro-form)) (.flatten acro-form)))     ;convert editable fields into values
    doc))

(defn- build-position-data [pdf-doc & [[start-page end-page]]]
  (let [stripper (cond-> (in-memory-text-stripper)
                         start-page (doto (.setStartPage start-page))
                         end-page (doto (.setEndPage end-page)))]
    (.getText stripper pdf-doc)
    (.getData stripper)))

(defn- build-line-position-data [pdf-doc & [[start-page end-page]]]
  (->> (range (or start-page 0) (or end-page (.getNumberOfPages pdf-doc)))
       (map (fn [page-no] (let [page (.getPage pdf-doc page-no)
                                stripper (in-memory-line-stripper page)]
                            (.processPage stripper page)
                            (map #(assoc % :page-number (inc page-no)) (.getData stripper)))))
       flatten))

(defn bookmark->map [^PDOutlineItem bookmark]
  (assoc
    (if (instance? PDActionGoTo (.getAction bookmark))
      (if-let [dest (-> bookmark .getAction .getDestination)]
        {:page-number (inc (.retrievePageNumber dest))
         :y           (try (max (- (-> dest .getPage .getMediaBox .getHeight)
                                   (.getTop dest))
                                0)
                           (catch Exception _ 0))}))
    :name (.getTitle bookmark)))

(defn node-recur
  ([node] (node-recur node false))
  ([node root?] {:node     (if root? "root" (bookmark->map node))
                 :children (mapv node-recur (.children node))}))

(defn get-bookmarks [pdf-doc & _]
  (if-let [outline (-> pdf-doc .getDocumentCatalog .getDocumentOutline)]
    (:children (node-recur outline true))))

(defn- shorten-doc [istream [start-page end-page] output-file]
  (with-open [doc (-> istream io/input-stream RandomAccessBufferedFileInputStream. PDFParser. (doto (.parse)) .getPDDocument preprocess-pdf)]
    (dorun (for [p (concat (repeat (- (.getNumberOfPages doc) (inc end-page)) (inc end-page)) (range 0 start-page))]
             (.removePage doc p)))
    (.save doc output-file)))

(defn- get-annotations [doc & _]
  (let [rect->edn (fn [^PDRectangle ann top-y]
                    (let [y0 (- top-y (.getUpperRightY ann))]
                      {:x0 (Math/floor (.getLowerLeftX ann))
                       :x1 (Math/ceil (.getUpperRightX ann))
                       :y0 y0
                       :y1 (+ y0 (- (.getUpperRightY ann) (.getLowerLeftY ann)))}))
        pojo->edn (fn [^PDAnnotation ann top-y]
                    (assoc (rect->edn (.getRectangle ann) top-y) :color (some-> ann .getColor .getComponents vec)))]
    (flatten (for [page-no (range 0 (.getNumberOfPages doc))]
               (let [^PDPage page (.getPage doc page-no)
                     top-y (-> page .getCropBox .getUpperRightY)]
                 (keep #(assoc (pojo->edn % top-y) :page-number (inc page-no)) (.getAnnotations page)))))))

(defn- page-characteristics [doc & _]
  (for [page-no (range 0 (.getNumberOfPages doc))]
    (let [^PDPage page (.getPage doc page-no)
          crop-box (.getCropBox page)]
      (as-> {:page-number (inc page-no) :y0 (.getLowerLeftY crop-box)  :x0 (.getLowerLeftX crop-box)
             :y1         (.getUpperRightY crop-box)  :x1 (.getUpperRightX crop-box)} pg
            (assoc pg :orientation (if (> (:x1 pg) (:y1 pg)) :landscape :portrait))))))

;; PUBLIC Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn process-pdf-document [process-fn is & pages]
  (with-open [doc (-> is io/input-stream RandomAccessBufferedFileInputStream. PDFParser. (doto (.parse)) .getPDDocument)]
    (apply process-fn (cons (preprocess-pdf doc) pages))))

(def extract-text-positions (partial process-pdf-document build-position-data))
(def extract-graphics (partial process-pdf-document build-line-position-data))
(def extract-bookmarks (partial process-pdf-document get-bookmarks))
(def extract-annotations (partial process-pdf-document get-annotations))
(def extract-page-characteristics (partial process-pdf-document page-characteristics))