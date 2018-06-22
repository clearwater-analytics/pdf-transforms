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

;TODO worry about efficiency later (maybe reduce the bounds as we walk through the page)
; by returns the new boundaries (input - everything above this token) and if there is a boundary to the right
(defn boundary-to-right? [boundaries {:keys [x width y height]} {right-token-x :x}]
  (some (fn [{:keys [x0 x1 y0 y1 boundary-axis]}]
          (and (= :x boundary-axis)                         ;The boundary separates along the x axis
               (>= x0 (+ x width))                          ;bound starts after the end of the left token
               (>= right-token-x x1)                        ;bound ends before the start of the right token
               (or (and (> (- y1 3) y)                      ;bound ends well after token ends
                        (> y y0))                           ;bound starts before the token ends
                   (and (> (- y height 3) y0)               ;bound starts after the token begins
                        (> y1 (- y height))))))                  ;bound ends after the token begins
        boundaries))

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

(defn compose-segments [tokens & [visual-boundaries]]
  (->> tokens
       utils/create-lines
       (mapcat (partial utils/partition-when (fn [tkn-a tkn-b]
                                               (or
                                                 (line-break? tkn-a tkn-b)
                                                 (boundary-to-right? visual-boundaries tkn-a tkn-b)))))
       (map decorate-segment)))
