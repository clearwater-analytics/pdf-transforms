(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.blocks.core :as b]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.core :as core]
            [sandbox.utils :as u]
            [clojure.string :as s]))


;TODO add a 'center of mass' comparison check (to test alignment difference between block and other words)
; maybe that would be useful, or just spin up a classifier
(def tokens->basic-blocks
  (partial cmn/compose-groups
           {:terminates? (fn [block stream]
                           (and (or
                                  (:horizontal-bar? (first stream))
                                  (re-matches utils/dash-line (:text (first stream)))
                                  (re-matches #".*(?:[:]|[.]{2,})" (:text (peek block))) ;added
                                  (b/y-gap? block (first stream)))
                                (not (b/x-gap? block stream))))
            :irrelevant? (fn [block stream]
                           (or (b/font-switch-newline? block (first stream))
                               (b/x-gap? block stream)))
            :add-to      (fn [block word] (conj (or block []) word))}))


(defn parse-blocks [pdf-path]
  (->> (core/pages-of-words pdf-path [0 100])
       (mapcat tokens->basic-blocks)
       (map #(assoc (cmn/boundaries-of %) :content %))))

(defn annotate-blocks [pdf-path & [{:keys [out]}]]
  (->> pdf-path
       parse-blocks
       (a/annotate {:pdf-url pdf-path :output-directory (or out u/annotated-dir)})
       dorun))

#_(annotate-blocks (str "file:" u/home-dir "/temp.pdf"))


#_(->> (str u/home-dir "/Documents/pdf_parsing/control_1/raw")
       u/get-pdfs-in-dir
       (map #(annotate-blocks % {:out (str u/home-dir "/Documents/pdf_parsing/control_1/tagged")}))
       dorun)











;;;;;;;;;;;; OLDER STUFF  ;;;;;;;;;;

(def delimiter-ending #".*(?:[.]{2,}|\-+|\:)")

(defn new-segment? [{ax :x ap :page-number :as a}
                    {bx :x bp :page-number :as b}]
  (or
    (> ax bx)
    (utils/gap? a b)
    (utils/new-line? a b)
    (not= ap bp)))



;TODO try to learn a (linear) classifier in place of new-segment?
; additional features might be the types of the data, the actual x distance between them (instead of the gap? call)

#_(core/annotate-components (str "file:" u/dir "title-table-side-by-side.pdf"))

#_(->> u/dir
       u/get-pdfs-in-dir
       (map #(->> (pe/extract-char-positions %)
                  pd/->pages-of-words
                  (apply concat)
                  (utils/partition-when new-segment?)
                  (map (comp (fn [x] (assoc x :class :table)) cmn/boundaries-of))
                  (a/annotate {:pdf-url % :output-directory u/annotated-dir})
                  dorun))
       dorun)