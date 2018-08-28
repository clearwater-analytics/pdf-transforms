(ns sandbox.regression-tests
  (:require [pdf-transforms.core :as c]
            [sandbox.utils :as u]
            [clojure.edn :as edn]
            [clojure.string :as s]))

(defmulti format-for-tests (fn [data-format & _] data-format))

(defmethod format-for-tests :default [_ segment]
  (select-keys segment [:text :x0 :x1 :y0 :y1]))

(defn pretty-format [data-format data]
  (format-for-tests data-format
                    (-> data (update :x0 int) (update :x1 int) (update :y0 int) (update :y1 int))))

(def url-base (str "file:" u/home-dir "/Documents/pdf_parsing/"))

(defn filename->input-url [control-set file-name]
  (str url-base control-set "/raw/" file-name  ".pdf"))

(defn expect-file [control-set format file-name]
  (str url-base control-set "/expectations/" file-name "-" (name format) ".edn"))

(defn load-expected [control-set format file-name]
  (-> (expect-file control-set format file-name) slurp edn/read-string))

(defn file->expected-format [control-set format file-name]
  (map (partial pretty-format format)
    (c/transform (filename->input-url control-set file-name) {:format format})))

(defn store-expected [control-set format file-name]
  (spit (expect-file control-set format file-name) (pr-str (file->expected-format control-set format file-name))))

(def test-filenames (->> (u/get-pdfs-in-dir (str u/home-dir "/Documents/pdf_parsing/control_2/raw/"))
                         (map (comp #(s/replace % ".pdf" "") last #(s/split % #"/")))))

(defn segment-changes []
  (->> test-filenames
       (map
         (comp
           (fn [{:keys [actual expected] :as x}]
             (assoc x :changes (filter identity (map #(when-not (= %1 %2) %2) actual expected))))
           #(array-map :file %
                       :expected (load-expected "control_2" :segments %)
                       :actual (file->expected-format "control_2" :segments %))))
       (filter (comp seq :changes))
       (map #(select-keys % [:file :changes]))))


(defn blocks-changes []
  (->> test-filenames
       (map
         (comp
           (fn [{:keys [actual expected] :as x}]
             (assoc x :changes (filter identity (map #(when-not (= %1 %2) %2) actual expected))))
           #(array-map :file %
                       :expected (load-expected "control_2" :blocks %)
                       :actual (file->expected-format "control_2" :blocks %))))
       (filter (comp seq :changes))
       (map #(select-keys % [:file :changes]))))
