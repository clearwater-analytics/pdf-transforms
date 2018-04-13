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


#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_2/")]
  (->> (str base-dir "raw")
       u/get-pdfs-in-dir
       (map #(annotate-blocks % {:out (str base-dir "blocks")}))
       dorun))











;;;;;;;;;;;; SEGMENTS  ;;;;;;;;;;


     ;;;;;  start   GAPS    ;;;;;
(def data-rgx #".*(?:\d|[?$%]).*")
(def word-rgx #"(?:(?:[a-zA-Z]*[aeiou][a-z]*)|(?:[aiouAIOU][a-z]?))\,?\.?")

;TODO cases  -  non-cap char -> Cap character
; , on either side (thanks edgar)
; : or -  ... maybe ... ?

;TODO unrelated to gap, but might want to filter out garbage words (transparent, _____, no text)


(defn gap? [{x0 :x w0 :width t0 :text y0 :y fsize1 :f-size :as word1}
            {x1 :x w1 :width t1 :text y1 :y fsize2 :f-size :as word2}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* (cond
           (and (b/new-font? word1 word2)                   ;between words with differing font and y vals (side by side components!)
                (> (Math/abs (- y0 y1)) 0.5))
           2.0
           (or (s/ends-with? t0 ",")
               (s/starts-with? t1 ","))
           5.5
           (and (re-matches word-rgx t0)                    ;between english words, optional period
                (re-matches word-rgx t1))
           3.0
           (and (re-matches #".*\." t0)                    ;between english words, optional period
                (re-matches word-rgx t1))
           3.0
           (and (re-matches data-rgx t0)                    ;between 'data'
                (re-matches data-rgx t1))
           1.25
           (re-matches data-rgx t0) ;between 'data' and something else
           1.75
           ;(and (re-matches word-rgx t0)
           ;     (re-matches #"[A-Z]" t1))
           ;2.0
           :else 2.0)
         (min (/ w0 (max 1 (count t0)))
              (/ w1 (max 1 (count t1)))))))

#_(defn debug-gap! [{x0 :x w0 :width t0 :text y0 :y :as word1}
                  {x1 :x w1 :width t1 :text y1 :y :as word2}]
  (let [diff (- x1 (+ x0 w0))
        avg-char-size (min (/ w0 (max 1 (count t0)))
                           (/ w1 (max 1 (count t1))))]
    {:diff diff
     :avg-char-size avg-char-size
     :ratio (/ diff avg-char-size)
     :multiplier-class (cond
                         (and (b/new-font? word1 word2)                   ;between words with differing font and y vals (side by side components!)
                              (> (Math/abs (- y0 y1)) 0.5))
                         :diff-font-and-ys
                         (or (s/ends-with? t0 ",")
                             (s/starts-with? t1 ","))
                         :commas
                         (and (re-matches word-rgx t0)                    ;between english words, optional period
                              (re-matches word-rgx t1))
                         :two-words
                         (and (re-matches #".*\." t0)                    ;between english words, optional period
                              (re-matches word-rgx t1))
                         :period-n-word
                         (and (re-matches data-rgx t0)                    ;between 'data'
                              (re-matches data-rgx t1))
                         :data-n-data
                         (re-matches data-rgx t0) ;between 'data' and something else
                         :data-n-anything
                         :else :else)}))


(defn new-segment? [{ax :x :as a} {bx :x :as b}]
  (or (> ax bx) (gap? a b) (utils/new-line? a b)))

;;;;;;   end GAPS   ;;;;;;;;


(defn new-alignment [b1 b2]
  (or
    (and ())
    )


  )






(def segment-decor-graph
  {:text-list         (fnk [tokens] (map :text tokens))
   :text              (fnk [text-list] (s/join " " text-list))
   :avg-x             (fnk [x0 x1] (/ (+ x1 x0) 2.0))
   :num-tokens        (fnk [tokens] (count tokens))
   :width             (fnk [x0 x1] (int (- x1 x0)))
   :height            (fnk [y0 y1] (int (- y1 y0)))
   ;:bold-ratio        (fnk [tokens num-tokens] (/ (count (filter :bold? tokens)) num-tokens))
   ;:italic-ratio      (fnk [tokens num-tokens] (/ (count (filter :italic? tokens)) num-tokens))
   ;:all-caps-ratio    (fnk [text-list num-tokens] (/ (count (filter (partial re-matches utils/all-caps) text-list)) num-tokens))
   ;:num-english-words (fnk [text-list] (->> text-list
   ;                                         (keep (comp
   ;                                                 (partial re-matches utils/eng-wordy)
   ;                                                 #(clojure.string/replace % utils/punctuation "")))
   ;                                         count))
   ;:num-datapoints    (fnk [text-list] (->> text-list
   ;                                         (keep (partial re-matches utils/datapoint))
   ;                                         count))
   :font              (fnk [tokens] (->> tokens (map (comp b/font-clean :font)) frequencies (apply max-key second) first))
   :font-size         (fnk [tokens] (->> tokens (map :f-size) frequencies (apply max-key second) first))
   })


(def decorate-segment (graph/compile segment-decor-graph))



(defn sample-blocks [n]
  (->> (pe/extract-char-positions (str "file:" u/home-dir "/Documents/pdf_parsing/control_2/raw/866c354c846ed29c9d415dd6066aecd8.pdf"))
       pd/->pages-of-words
       (mapcat (comp
                 (partial map (comp #(dissoc % :tokens :text-list)
                                    #(merge % (decorate-segment %))
                                    #(assoc (cmn/boundaries-of %) :tokens %)))
                 (partial utils/partition-when new-segment?)))
       (take n)))

(defn y-gaps [{:keys [y1]} blocks]
  (:gaps (reduce (fn [{:keys [gaps prev-y1]} {:keys [y0 y1]}]
                   {:gaps    (conj gaps (- y0 prev-y1))
                    :prev-y1 y1})
                 {:gaps [] :prev-y1 y1} blocks)))

#_(defn interblock-y-gap? [{:keys [y0 y1 line-segments] :as block} blocks-below]
  (let []
    (if (> (count line-segments) 1)


      ;check average line diff,

      )
    #_(if (pos? avg-block-diff)
        (> (- y y0 (if ss? height0 0)) (* 1.25 avg-block-diff))
        (> (- y height)
           (+ y0 (* (cond ss? 4.0
                          (= (font-clean font) (font-clean font2)) 2.0
                          :else 1.5)
                    (min height0 height)))))

    ))

(defn group-vertically [block other-blocks]
  (let [below (some->> other-blocks
                      (filter #(= #{:below} (cmn/relative-to block %)))
                      (sort-by :y0))]


    ; criteria (might add features to the blocks for this)
    ; same font and font size (When cleaned)
    ; y gap is not too large
    ; y gap between this line and the next line is smaller or roughly equal to that between the next line and the one after it
    ; horizontal alignment is the same (account for tabbed paragraph starts)
    ; returns a vector of form [block remaining-blocks]

    )

  ;TODO need to account for font changes,
  )


#_(some->> blocks
         (filter #(= #{:below} (cmn/relative-to table %)))
         not-empty
         (sort-by :y0)
         (cmn/compose-groups
           {:terminates? terminate-down?
            :irrelevant? (fn [& _] false)
            :add-to      (fn [component block]
                           (if component
                             (update component :blocks #(conj % block))
                             {:type :table :blocks [block]}))})
         first
         :blocks
         not-empty
         (valid-extension table)
         (mapcat :content)
         cmn/boundaries-of)


;TODO implement algorithm
; grab 2 segments directly below this one

;TODO can worry about performance later
(defn merge-segments-vertically [segments]
  (loop [blocks []
         remaining segments]
    (let [[block outside-of] (group-vertically (first remaining) (rest remaining))]
      (recur (conj blocks block) outside-of))))

#_(defn annotate-segments [pdf-url & [{:keys [out]}]]
  (->> (pe/extract-char-positions pdf-url)
       pd/->pages-of-words
       (mapcat (comp
                 merge-segments-vertically
                 (partial map #(assoc (cmn/boundaries-of %) :line-segments [%]))
                 (partial utils/partition-when new-segment?)))
       (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)})))

(defn annotate-segments [pdf-url & [{:keys [out]}]]
  (->> (pe/extract-char-positions pdf-url)
       pd/->pages-of-words
       (mapcat (comp
                 (partial map #(assoc (cmn/boundaries-of %) :line-segments [%] :class :generic))
                 (partial utils/partition-when new-segment?)))
       (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)})))



#_(let [base-dir (str u/home-dir "/Documents/pdf_parsing/control_gap/")]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(annotate-segments % {:out (str base-dir "segments")}))
         dorun))
