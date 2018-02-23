(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.components.core :as cmps]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.blocks.core :as blks]
            [pdf-transforms.blocks.features :as f]
            [pdf-transforms.blocks.classify :as cls]
            [pdf-transforms.core :as core]
            [clojure.java.io :as io]
            [sandbox.utils :as u]))

;;;  CURRENT STUFF  ;;;
(def delimiter-ending #".*(?:[.]{2,}|\-+|\:)")

(defn new-segment2? [{ax :x atext :text ap :page-number :as a}
                     {bx :x bp :page-number :as b}]
  (or
    (> ax bx)
    (utils/gap? a b)
    (utils/new-line? a b)
    (not= ap bp)
    #_(blks/new-font? a b)
    #_(re-matches delimiter-ending atext)))



;TODO try to learn a (linear) classifier in place of new-segment2?
; additional features might be the types of the data, the actual x distance between them (instead of the gap? call)


#_(->> u/dir
       u/get-pdfs-in-dir
       (map #(->> (pe/extract-char-positions %)
                  pd/->pages-of-words
                  (apply concat)
                  (utils/partition-when new-segment2?)
                  (map (comp (fn [x] (assoc x :class :table)) cmn/boundaries-of))
                  (a/annotate {:pdf-url % :output-directory u/annotated-dir})
                  dorun))
       dorun)


(->> u/dir
     u/get-pdfs-in-dir
     (map #(core/annotate-blocks % {:output-directory u/annotated-dir}))
     dorun)







;;;;;;  END CURRENT  ;;;;;;;








(defn new-segment? [{ax :x ap :page-number :as a} {bx :x bp :page-number :as b}]
  (or
    (> ax bx)
    (utils/gap? a b)
    (utils/new-line? a b)
    (not= ap bp)))


(defmulti label-words-in :type)
(defmethod label-words-in :table [{[labels & labeled] :vals}]
  [(->> labels flatten (map (comp #(assoc % :class :label) u/token->bounds)))
   (->> labeled flatten (map (comp #(assoc % :class :labeled) u/token->bounds)))])

(defmethod label-words-in :default [{:keys [vals]}]
  (->> vals flatten (map (comp #(assoc % :class :other) u/token->bounds))))

(defn components->labelz [components]
  (->> components
       (map label-words-in)
       flatten))

;TODO merge in manual corrections

(defn label-data [doc-id pdf-url]
  (->> (core/transform pdf-url {:page-bounds [0 10] :format :components})
       components->labelz
       (a/annotate {:pdf-url pdf-url :output-directory u/annotated-dir :drawn-ids? true})
       (remove #(= :other (:class %)))
       (map (fn [{:keys [id class]}]
              {:token-id id :class class :doc-id doc-id}))))














(comment


  (->> dir
       get-pdfs-in-dir
       (map label-data)
       dorun)



  ;words
  (->> dir
       get-pdfs-in-dir
       (map #(->> (pe/extract-char-positions % [0 10])
                  pd/->pages-of-words
                  (apply concat)
                  (a/annotate {:pdf-url % :output-directory annotated-dir})
                  dorun))
       dorun)

  ;line segments
  (->> dir
       get-pdfs-in-dir
       (take 3)
       (map #(->> (pe/extract-char-positions % [0 10])
                  pd/->pages-of-words
                  (apply concat)
                  (utils/partition-when new-segment2?)
                  (map cmn/boundaries-of)
                  (a/annotate {:pdf-url % :output-directory annotated-dir})
                  dorun))
       dorun)

  ;lines
  (->> dir
       get-pdfs-in-dir
       (map #(->> (pe/extract-char-positions % [0 10])
                  pd/->pages-of-words
                  (mapcat utils/create-lines)
                  (map cmn/boundaries-of)
                  (a/annotate {:pdf-url % :output-directory (str annotated-dir "line_segs/")})
                  dorun))
       dorun)


  (->> dir
       get-pdfs-in-dir
       (map #(core/annotate-blocks % {:output-directory (str annotated-dir "blocks/")}))
       dorun)


  )
