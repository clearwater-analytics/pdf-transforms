(ns pdf-transforms.blocks.core
  "Functions for grouping words into semantic chunks, where whitespace and
  font changes are used to infer boundaries."
  (:require [clojure.string :as s]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]))

(def font-noise #"[A-Z]+[+]|[Ii][Tt][Aa][Ll][iI][cC][sS]?|[=,-.]")

(defn font-clean [font]
  (s/replace font font-noise ""))

(defn new-font? [{font2 :font f-size2 :f-size} {:keys [font f-size]}]
  (or (not= (font-clean font) (font-clean font2))
      (not= f-size f-size2)))

(defn font-switch? [block token]
  (new-font? token (peek block)))

(defn font-switch-newline? [block {y1 :y :as word}]
  (let [{y0 :y height :height ss? :superscript?} (peek block)]
    (and (font-switch? block word)
         (not ss?)
         (> (- y1 y0) height))))

(defn avg-line-diff [words]
  (let [{:keys [cnt sum]}
        (->> words
             (remove :superscript?)
             (sort-by :y)
             (reduce (fn [{:keys [sum cnt y0]} {y1 :y h1 :height}]
                       (let [sig-diff? (> (- y1 y0) h1)]
                         {:y0  y1
                          :sum (+ sum (if sig-diff? (- y1 y0) 0))
                          :cnt (+ cnt (if sig-diff? 1 0))}))
                     {:sum 0 :cnt 0 :y0 1000}))]
    (/ sum (max 1 cnt))))

(defn y-gap? [block {:keys [font height y]}]
  (let [{y0 :y height0 :height font2 :font ss? :superscript?} (peek block)
        avg-block-diff (avg-line-diff block)]
    (if (pos? avg-block-diff)
      (> (- y y0 (if ss? height0 0)) (* 1.25 avg-block-diff))
      (> (- y height)
         (+ y0 (* (cond ss? 4.0
                        (= (font-clean font) (font-clean font2)) 2.0
                        :else 1.5)
                  (min height0 height)))))))

(defn last-in-segment [stream]
  (reduce (fn [{:keys [x y] :as prev} {x1 :x y1 :y :as word}]
            (if (or
                  (utils/gap? prev word)
                  (< x1 x)
                  (> (- y1 y) 3))
              (reduced prev)
              word))
          (first stream) (rest stream)))

(defn x-gap? [block stream]
  (or
    (utils/gap? (peek block) (first stream))
    (utils/gap? (last-in-segment stream) (first block))))

(def tokens->basic-blocks
  (partial cmn/compose-groups
           {:terminates? (fn [block stream]
                           (and (or
                                  (:horizontal-bar? (first stream))
                                  (re-matches utils/dash-line (:text (first stream)))
                                  (y-gap? block (first stream)))
                                (not (x-gap? block stream))))
            :irrelevant? (fn [block stream]
                           (or (font-switch-newline? block (first stream))
                               (x-gap? block stream)))
            :add-to      (fn [block word] (conj (or block []) word))}))

(defn adjust [{:keys [content] :as block0} [{x0 :x0} :as blocks]]
  (let [lines (utils/create-lines content)
        [title & columns] (utils/partition-when (fn [line1 line2]
                                                  (let [{x-l1 :x w-l1 :width} (last line1)
                                                        {x-l2 :x w-l2 :width} (last line2)]
                                                    (and
                                                      (> (+ x-l1 w-l1) (- x0 8))
                                                      (< (+ x-l2 w-l2) (- x0 8))))) lines)
        title-content (flatten title)
        column-content (flatten (drop (count title) lines))
        merge-blocks (fn [{:keys [content] :as blk}
                          {cntnt :content :as blk2}]
                       (assoc (utils/expand-bounds blk blk2) :content (apply conj content cntnt)))]
    (if (and (not-empty title) (not-empty columns)
             (every? (fn [{:keys [x width]}] (< (+ x width) x0)) column-content))
      (concat blocks
              [(assoc (cmn/boundaries-of title-content) :content title-content)
               (assoc (cmn/boundaries-of column-content) :content column-content)])
      [(reduce merge-blocks block0 blocks)])))


(defn adjust-overlapping [blocks]
  (loop [blks (cmn/sort-blocks blocks)
         result []]
    (if-let [blk (first blks)]
      (let [{overlap true independent false}
            (group-by (partial cmn/overlaps? blk) (rest blks))]
        (recur independent
               (apply conj result (if (empty? overlap) [blk]
                                                       (do #_(println "overlap"
                                                                      (map :text (:content blk))
                                                                      (map (comp (partial map :text) :content) overlap))
                                                         (adjust blk overlap))))))
      result)))

(defn blocks-on-page [page]
  (->> page
       tokens->basic-blocks
       (keep (comp
               not-empty
               (partial remove :horizontal-bar?)))
       (map #(assoc (cmn/boundaries-of %) :content %))
       adjust-overlapping
       cmn/sort-blocks))