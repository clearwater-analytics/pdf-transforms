(ns pdf-transforms.components.tables
  "Functions for composing table components from featured blocks."
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [pdf-transforms.utilities :as utils]
            [pdf-transforms.components.columnar :as cols]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.components.parse :as prs]))

(defn remove-dot-padding [word]
  (let [word-text (s/trim (:text word)) dot-cnt (count (re-find #"[.]{2,}" word-text))]
    (if (pos? dot-cnt)
      (cond
        (re-matches #"[.]{2,}" word-text)
        (assoc word :width 0 :text "")
        (re-matches #".+[.]{2,}" word-text)
        (assoc word :text (s/replace word-text #"[.]{2,}" " ") :width (- (:width word) (* 3 dot-cnt))) ;dot-width = 3
        (re-matches #"[.]{2,}.+" word-text)
        (assoc word :text (s/replace word-text #"[.]{2,}" " ") :x (+ (:x word) (* 3 dot-cnt)) :width (- (:width word) (* 3 dot-cnt)))
        :else
        word)
      word)))


(defn clean-up [row-data]
  (->> row-data
       (remove #(re-matches #"[.$]*" (s/replace (:text %) #"\s+" ""))) ;remove whitespaces, single periods, and $'s
       (map remove-dot-padding)))

(defn grouping? [{{:keys [num-tokens
                          num-lines]} :features
                  content             :content
                  :as                 this}
                 stream]
  (if num-tokens
    (and
      (< num-tokens 8)
      (= 1 num-lines)
      (re-matches utils/header-like (s/join " " (map :text content)))
      (let [nxt-item (first (filter #(= #{:below} (cmn/relative-to this %)) stream))]
        (and
          (cmn/data-column? nxt-item)
          (not (cmn/data-and-labels? nxt-item)))))))

(defn summary-row? [block stream]
  (some->> stream
           (filter #(= #{:right} (cmn/relative-to block %)))
           not-empty
           (map (comp
                  #(select-keys % [:num-datapoints :num-tokens])
                  :features))
           (apply merge-with +)
           ((fn [{:keys [num-datapoints num-tokens]}]
              (if (and num-tokens (pos? num-tokens))
                (>= (/ num-datapoints num-tokens) 0.5))))))

(def page-footer #"(?:[Pp][aAgGeE]{0,3}\s*[.]?\s*\d.*)|[-]?\s*\d{0,3}\s*[-]?")
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(defn page-footer? [{:keys [content]} stream]
  (and
    (re-matches page-footer (s/join " " (map :text content)))
    (<= (count stream) 2)))

(defn table-footer? [{:keys [content]}]
  (let [as-text (s/join " " (map :text content))]
    (or
      (re-matches utils/dash-line as-text)                  ;____ or ----- line
      (:superscript? (first content))                       ;line starts with a superscript
      (re-matches footnote as-text))))                      ;lines starts with what should be a superscript


(defn table-block? [_ [nxt-block & stream]]
  (or
    (cmn/data-column? nxt-block)
    (grouping? nxt-block stream)
    (summary-row? nxt-block stream)))

(defn ruins-columns? [{tbl-content :content} {blk-content :content}]
  (if tbl-content
    (let [empty-col-pixels (->> tbl-content utils/create-lines prs/column-bounds cols/columns-as-pixel-range)
          blk-occupied-pixels (->> blk-content
                                   (sort-by :x)
                                   (map (fn [{:keys [x width]}] {:x0 (int x) :x1 (int (+ x width))}))
                                   cols/columns-as-pixel-range)
          overlap (cset/intersection empty-col-pixels blk-occupied-pixels)]
      (> (/ (count overlap)
            (max (count empty-col-pixels) 1))  0.2))))

(defn terminate-down? [component stream]
  (let [nxt-block (first stream)]
    (or
      (page-footer? nxt-block stream)
      (table-footer? nxt-block)
      (not (table-block? component stream))
      (ruins-columns? component nxt-block))))

(defn ->table [words coords]
  (merge {:type :table
          :vals (prs/parse-table (->> coords (cmn/words-in-box words) clean-up utils/create-lines))}
         coords))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      NEW STUFF   THIS IS UNDER DEVELOPMENT        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-to-table [table {:keys [page-number] :as blk}]
  (-> table
      (assoc :page-number page-number)
      (utils/expand-bounds blk)
      (update :content #(conj % blk))))

(defn grab-headers-new [all-blocks {:keys [content] :as tbl}]
  (let [block (mapcat :content content)
        block-as-lines (utils/create-lines block)]
    (if-let [index (cmn/header-index block-as-lines)]
      (let [head-block (->> block-as-lines (take (inc index)) flatten cmn/boundaries-of)
            {:keys [x0 x1 y0]} (some->> all-blocks
                          (filter (fn [blk]
                                    (and
                                      (= #{} (cmn/relative-to tbl blk)) ;partially inline with other header row
                                      (let [rel (cmn/relative-to head-block blk)]
                                        (or (= #{:right} rel) (= #{:left} rel))))))
                          (mapcat :content)
                          cmn/boundaries-of)]
        {:y0 (apply min (or y0 1000) (map (fn [{:keys [y height]}] (- y height)) block))
         :y1 (apply max (map :y (nth block-as-lines index)))
         :x0 (or x0 1000) :x1 (or x1 0)})
      (let [header-row? (fn [blocks]
                          (>= (/ (count (filter identity (map utils/label-like? blocks)))
                                 (max (count blocks) 1))
                              0.5))
            tbl-xys (->> content (map (fn [{:keys [x0 y0]}] (str x0 "_" y0))) (into #{}))
            above-candidates (filter (fn [blk]
                                       (= #{:above} (cmn/relative-to tbl blk))) all-blocks)
            inline-candidates (filter
                                (fn [{bx0 :x0 by0 :y0 :as blk}]
                                  (and
                                    (= #{} (cmn/relative-to tbl blk))
                                    (not (tbl-xys (str bx0 "_" by0))))) all-blocks)
            lowest-above (some->> above-candidates not-empty (apply max-key :y1))
            above-headers (filter #(let [rel (cmn/relative-to lowest-above %)]
                                 (#{#{:right} #{:left} #{}} rel)) above-candidates)]
        (not-empty
          (reduce add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0} (cond
                                                                 (header-row? inline-candidates) inline-candidates
                                                                 (header-row? above-headers) above-headers)))))))


(defn valid-extension [table blocks]
  (loop [stream blocks]
    (if (empty? stream)
      blocks
      (if (not (terminate-down? table stream))
        (recur (rest stream))
        nil))))

(defn expand-down [table blocks]
  (if-let [expansion (some->> blocks
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
                              cmn/boundaries-of)]
    (utils/expand-bounds table expansion)
    table))

(defn blocks->table-boundary [blocks]
  (some->> blocks
           (filter cmn/table-column?)
           not-empty
           (#(reduce (fn [tables col]
                       (let [table (peek tables)]
                         (if (let [rel (cmn/relative-to table col)]
                               (or
                                 (= #{:right} rel)
                                 (= #{:left} rel)
                                 (= #{} rel)))
                           (conj (pop tables) (add-to-table table col))
                           (conj tables (add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0} col)))))
                     [(add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0} (first %))] (rest %)))
           (filter #(> (count (:content %)) 1))
           (keep #(let [{dx0 :x0 dx1 :x1 dy1 :y1 dy0 :y0 page-num :page-number tbl-blocks :content :as tbl} %
                        {:keys [x0 x1 y0 y1]} (grab-headers-new blocks tbl)]
                   (if y1
                     {:y0     (min dy0 y0)
                      :y1     dy1
                      :x0     (min dx0 x0)
                      :x1     (max dx1 x1)
                      :page-number page-num
                      :content (mapcat :content tbl-blocks)})))
           (map (comp
                  #(dissoc % :content)
                  #(expand-down % blocks)))
           (sort-by (fn [{:keys [x0 x1 y0 y1]}] (* -1.0 (- x1 x0) (- y1 y0)))) ;sort by area, descending
           (reduce (fn [uniques tbl]
                     (if (some #(cmn/within? % tbl) uniques)
                       uniques
                       (conj uniques tbl))) [])))

(defn sane-table? [{vals :vals}]
  (>= (count vals) 2))

(defn ->standard-tables [blocks page]
  (->> blocks
       blocks->table-boundary
       (map (partial ->table page))
       (filter sane-table?)))