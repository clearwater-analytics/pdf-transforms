(ns sandbox.core
  (:require [pdf-transforms.tokens.pdf-extractor :as pe]
            [pdf-transforms.tokens.positional-data :as pd]
            [pdf-transforms.blocks.core :as b]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.annotations :as a]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.core :as core]
            [sandbox.utils :as u]
            [clojure.string :as s]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]))


;TODO add a 'center of mass' comparison check (to test alignment difference between block and other words)
; maybe that would be useful, or just spin up a classifier
(def tokens->basic-blocks
  (partial cmn/compose-groups
           {:terminates? (fn [block stream]
                           (and (or
                                  (:horizontal-bar? (first stream))
                                  (re-matches utils/dash-line (:text (first stream)))
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

(def line-metrics-graph
  {:lines           (fnk [content] (utils/create-lines content))
   :pretty-text     (fnk [lines] (s/join "\n" (map (comp (partial s/join " ") (partial map :text)) lines)))
   :spacings        (fnk [lines] (if (= 1 (count lines))
                                   []
                                   (->> lines
                                        (map (comp first (partial remove :superscript?)))
                                        (partition 2 1)
                                        (map (fn [[{ay :y} {by :y bh :height}]] (- (+ by bh) ay))))))
   :spacing-avg     (fnk [spacings] (when (> (count spacings) 0) (/ (reduce + spacings) (count spacings))))
   :spacing-std-dev (fnk [spacings spacing-avg]
                      (when spacing-avg
                        (Math/sqrt (/ (reduce (fn [sum x]
                                                (+ sum (Math/pow (- spacing-avg x) 2.0))) 0 spacings)
                                      (count spacings)))))})

(def line-metrics (graph/compile line-metrics-graph))


(defn font-metrics [{:keys [content]}]
  {:font-sizes (into #{} (keep :f-size content))
   :size-mode  (ffirst (max-key second (frequencies (keep :f-size content))))
   :fonts      (into #{} (keep (comp b/font-clean :font) content))})

(defn add-metrics [block]
  (merge (line-metrics block) (font-metrics block) block))

(defn block-analysis [pdf-url]
  (->> pdf-url
       parse-blocks
       (a/annotate {:pdf-url pdf-url :output-directory u/annotated-dir})
       (map (comp #(dissoc % :lines :content) add-metrics)))

  )

#_(annotate-blocks (str "file:" u/home-dir "/temp.pdf"))


#_(->> (str u/home-dir "/Documents/pdf_parsing/control_1/raw")
       u/get-pdfs-in-dir
       (map #(annotate-blocks % {:out (str u/home-dir "/Documents/pdf_parsing/control_1/tagged")}))
       dorun)











;;;;;;;;;;;; OLDER STUFF  ;;;;;;;;;;

(defn new-segment? [{ax :x :as a} {bx :x :as b}]
  (or (> ax bx) (utils/gap? a b) (utils/new-line? a b)))

(defn annotate-segments [pdf-url]
  (->> (pe/extract-char-positions pdf-url)
       pd/->pages-of-words
       (mapcat (partial utils/partition-when new-segment?))
       (map cmn/boundaries-of)
       (a/annotate {:pdf-url pdf-url :output-directory u/annotated-dir})))



;TODO try to learn a (linear) classifier in place of new-segment?
; additional features might be the types of the data, the actual x distance between them (instead of the gap? call)

#_(core/annotate-components (str "file:" u/dir "title-table-side-by-side.pdf"))

#_(->> u/dir
       u/get-pdfs-in-dir
       (map annotate-segments)
       dorun)