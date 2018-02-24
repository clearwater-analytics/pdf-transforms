(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.core :as core]
            [sandbox.utils :as u]))


(def delimiter-ending #".*(?:[.]{2,}|\-+|\:)")

(defn new-segment? [{ax :x ap :page-number :as a}
                     {bx :x bp :page-number :as b}]
  (or
    (> ax bx)
    (utils/gap? a b)
    (utils/new-line? a b)
    (not= ap bp)))



;TODO try to learn a (linear) classifier in place of new-segment2?
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


#_(->> u/dir
     u/get-pdfs-in-dir
     (map #(core/annotate-blocks % {:output-directory u/annotated-dir}))
     dorun)


