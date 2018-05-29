(ns pdf-transforms.blocks.segments
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [clojure.string :as s]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.utilities :as utils]))

;TODO handle this (breaks non-overlapping rectangle assumption ...)
; (i)    (A) Government Rating Agency of any state of the
; payment of which is secured by an



(def data-rgx #".*(?:\d|[?$%]).*")
(def word-rgx #"(?:(?:[a-zA-Z]*[aeiou][a-z]*)|(?:[aiouAIOU][a-z]?))\,?\.?")
(def loose-word-rgx #"\S?[a-zA-Z]*[aeiouAEIOU][a-zA-Z]*\S?")
(def font-noise #"[A-Z]+[+]|[Ii][Tt][Aa][Ll][iI][cC][sS]?|[=,-.]")


(defn font-clean [font]
  (s/replace font font-noise ""))

(defn new-font? [{font2 :font font-size2 :font-size} {:keys [font font-size]}]
  (or (not= (font-clean font) (font-clean font2))
      (not= font-size font-size2)))


#_(defn horizontal-gap? [{x0 :x w0 :width t0 :text y0 :y fsize1 :font-size :as word1}
                       {x1 :x w1 :width t1 :text y1 :y fsize2 :font-size :as word2}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* (cond
           (and (new-font? word1 word2)                   ;between words with differing font and y vals (side by side components!)
                (> (Math/abs (- y0 y1)) 0.5))
           2.0
           (or (s/ends-with? t0 ",")
               (s/starts-with? t1 ",")
               (= "$" t0))
           5.5
           (and (re-matches loose-word-rgx t0)              ;between english words, optional period
                (re-matches loose-word-rgx t1))
           3.0
           (and (re-matches #".*\." t0)                     ;between period and next sentence
                (re-matches loose-word-rgx t1))
           3.0
           (and (re-matches data-rgx t0)                    ;between 'data'
                (re-matches data-rgx t1))
           1.25
           (re-matches data-rgx t0)                         ;between 'data' and something else
           1.75
           :else 2.0)
         (min (/ w0 (max 1 (count t0)))
              (/ w1 (max 1 (count t1)))))))

(defn horizontal-gap? [{x0 :x w0 :width t0 :text y0 :y fsize1 :font-size :as word1}
                       {x1 :x w1 :width t1 :text y1 :y fsize2 :font-size :as word2}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* (cond
           (and (new-font? word1 word2)                   ;between words with differing font and y vals (side by side components!)
                (> (Math/abs (- y0 y1)) 0.5))
           2.0
           (or (s/ends-with? t0 ",")
               (s/starts-with? t1 ",")
               (= "$" t0))
           5.5

           (re-matches #"[a-zA-Z].*\..{0,3}" t0)                                  ;sentence endings are problematic
           5.0
           :else 3.5)
         (min (/ w0 (max 1 (count t0)))
              (/ w1 (max 1 (count t1)))))))


(defn line-break? [{ellipsis-a? :ellipsis? :as a} {ellipsis-b? :ellipsis? :as b}]
  (or (horizontal-gap? a b) ellipsis-a? ellipsis-b?))

(defn new-segment? [{ax :x :as a} {bx :x :as b}]
  (or (> ax bx) (line-break? a b) (utils/new-line? a b)))

(defn split-on-gaps [line-of-tokens]
  (let [splits (utils/partition-when line-break? line-of-tokens)]
    splits
    ))


(def segment-decor-graph
  {:text                 (fnk [tokens] (s/join " " (map :text tokens)))
   :height               (fnk [y0 y1] (- y1 y0))
   :font                 (fnk [tokens] (->> tokens (map (comp font-clean :font)) frequencies (apply max-key second) first))
   :font-size            (fnk [tokens] (->> tokens (map :font-size) frequencies (apply max-key second) first))})

(def decorate (graph/compile segment-decor-graph))

(defn decorate-segment [tokens]
  (-> tokens
      cmn/boundaries-of
      (assoc :tokens tokens)
      ((juxt identity decorate))
      ((partial apply merge))))

(defn compose-segments [page-of-tokens]
  (->> page-of-tokens
       utils/create-lines
       (mapcat split-on-gaps)
       (map decorate-segment)))


#_(defn compose-segments [page-of-tokens]
  (->> page-of-tokens
       (utils/partition-when new-segment?)
       (map decorate-segment)))


