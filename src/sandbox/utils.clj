(ns sandbox.utils
  (:require [clojure.java.io :as io]))


(defonce home-dir (System/getProperty "user.home"))

(defn get-pdfs-in-dir [dir]
  (->> dir
       io/as-file
       file-seq
       rest
       (map (comp str #(.toURL %)))
       (filter #(clojure.string/ends-with? % ".pdf"))))

(def dir (str home-dir "/Documents/pdf_parsing/control_2"))
(def annotated-dir (str home-dir "/Temp/"))

(defn token->bounds [{:keys [x width y height page-number id]}]
  {:x0 x :x1 (+ x width) :y0 (- y height) :y1 y :page-number page-number :id id})