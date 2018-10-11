(ns pdf-transforms.blocks.new-features
  "Functions to determine feature values and to associate blocks with their feature values"
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]
            [clojure.string :as s]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]))

(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")
(def word-like #"[^\d]*[aeiouyAEIOUY]+[^\d]*")
(def MAX-GAP 1000)

(def vowel? #{\a \e \i \o \u \y})
(def consonant? #{\q \w \r \t \p \s \d \f \g \h \j \k \l \z \x \c \v \b \n \m})
(def digit? #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})
(def other? identity)


(defn token-summary [string]
  (->> string
       s/lower-case
       (take 10)
       (map (apply some-fn (map (partial apply (fn [class pred] #(when (pred %) class))) [[[1 0 0 0] vowel?]
                                                                                          [[0 1 0 0] consonant?]
                                                                                          [[0 0 1 0] digit?]
                                                                                          [[0 0 0 1] other?]])))
       (#(concat % (repeat 10 [0 0 0 0]))) ;pad to make fixed width
       (take 10)))


;can blocks have a more raw representation?
(def internal-features-graph
  {:lines                (fnk [tokens] (utils/create-lines tokens))
   :text-list            (fnk [tokens] (map :text tokens))
   :text                 (fnk [text-list] (s/join " " text-list))

   :num-tokens           (fnk [tokens] (count tokens))
   ;:horizontal-alignment (fnk [page-x0 page-x1 x0 x1]
   ;                        (cond
   ;                          (cmn/centered? page-x0 page-x1 {:x0 x0 :x1 x1}) :center
   ;                          (utils/within-x? 25 x0 page-x0) :left
   ;                          :else :float))
   :dist-from-left          (fnk [page-x0 x0] (- x0 page-x0))
   :dist-from-right         (fnk [page-x1 x1] (- page-x1 x1))
   :width                (fnk [x1 x0] (int (- x1 x0)))
   :height               (fnk [y0 y1] (int (- y1 y0)))
   :bold-ratio           (fnk [tokens] (/ (count (filter :bold? tokens)) (count tokens)))
   :italic-ratio         (fnk [tokens] (/ (count (filter :italic? tokens)) (count tokens)))
   :all-caps-ratio       (fnk [text-list]
                           (/ (count (filter (partial re-matches utils/all-caps) text-list)) (count text-list)))
   :ends-with-period?    (fnk [text-list] (boolean (re-matches utils/sentence-ending (last text-list))))
   :num-english-words    (fnk [tokens]
                           (->> tokens (remove :superscript?) (map :text) (filter (partial re-matches word-like)) count))
   :num-datapoints       (fnk [tokens]
                           (->> tokens (remove :superscript?) (map :text) (filter (partial re-find #"\d")) count))
   :num-lines            (fnk [tokens] (:sum (reduce (fn [{:keys [sum prev-y]} {y :y}]
                                                       {:sum    (+ sum (if (> (- y prev-y) 4) 1 0))
                                                        :prev-y y})
                                                     {:sum 1 :prev-y 1000} (sort-by :y tokens))))
   :num-sentences        (fnk [tokens] (->> tokens
                                            (remove :superscript?)
                                            (map :text)
                                            (filter (partial re-matches utils/sentence-ending))
                                            count))
   :keyword-start?       (fnk [text-list] (boolean (some (partial re-matches utils/delimited) (take 10 text-list))))
   :itemized-start?      (fnk [text tokens]
                           (boolean (or (:superscript? (first tokens)) ;line starts with a superscript
                                        (re-matches footnote text))))
   ;:first-token-summary    (fnk [tokens] (-> tokens first :text token-summary))

   ;composed features
   :data-ratio   (fnk [num-datapoints num-tokens] (/ num-datapoints num-tokens))
   :word-ratio   (fnk [num-english-words num-tokens] (/ num-english-words num-tokens))
   :tokens-per-line (fnk [num-tokens num-lines] (/ num-tokens num-lines))
   :avg-word-cnt-per-line (fnk [num-english-words num-lines] (/ num-english-words num-lines))
   :avg-data-cnt-per-line (fnk [num-datapoints num-lines] (/ num-datapoints num-lines))


   })

(def internal-features (graph/compile internal-features-graph))


(defn x-aligned-bits? [{page-x0 :x0 page-x1 :x1}
                       {ax0 :x0 ax1 :x1}
                       {bx0 :x0 bx1 :x1}]
  (let [acenter (/ (+ ax0 ax1) 2)
        bcenter (/ (+ bx0 bx1) 2)
        page-width (- page-x1 page-x0)]
    (and                                                    ;we are not interested in full page sections
      (< (/ (- ax1 ax0) page-width) 0.5)
      (< (/ (- bx1 bx0) page-width) 0.5)
      (or
        (utils/within-x? 10 ax0 bx0)
        (utils/within-x? 10 ax1 bx1)
        (utils/within-x? 10 acenter bcenter)))))


(defn context-features [other-blocks {:keys [x0 x1 y0 y1] :as block}]
  (let [page-coords {:x0 (apply min (map :x0 other-blocks))
                     :x1 (apply max (map :x1 other-blocks))}
        relatives (map #(assoc % :relative (cmn/relative-to block %)) other-blocks)
        format (fn [items] (->> items (sort-by (partial utils/euc-distance block)) (take 2)))
        directly-above (->> relatives
                            (filter (comp #(= % #{:above}) :relative))
                            reverse
                            (take-while (partial x-aligned-bits? page-coords block))
                            format)
        directly-below (->> relatives
                            (filter (comp #(= % #{:below}) :relative))
                            (take-while (partial x-aligned-bits? page-coords block))
                            format)
        to-right (format (filter (comp #(= % #{:right}) :relative) relatives))
        to-left (format (filter (comp #(= % #{:left}) :relative) relatives))]
    {:num-blocks-above          (count (filter (comp :above :relative) relatives))
     :num-blocks-directly-above (count directly-above)
     :num-blocks-below          (count (filter (comp :below :relative) relatives))
     :num-blocks-directly-below (count directly-below)
     :num-blocks-right          (count to-right)
     :num-blocks-left           (count to-left)

     :gap-right                 (if (seq to-right) (- (:x0 (first to-right)) x1) MAX-GAP)
     :gap-left                  (if (seq to-left) (- x0 (:x1 (first to-left))) MAX-GAP)
     :gap-above                 (if (seq directly-above) (- y0 (:y1 (first directly-above))) MAX-GAP)
     :gap-below                 (if (seq directly-below) (- (:y0 (first directly-below)) y1) MAX-GAP)

     :blocks-directly-below (map :idx directly-below)
     :blocks-directly-above (map :idx directly-above)
     :blocks-left (map :idx to-left)
     :blocks-right (map :idx to-right)}))


(defn- core-features [{:keys [context-features internal-features]}]
  (merge internal-features (dissoc context-features :blocks-right :blocks-left :blocks-directly-above :blocks-directly-below)))


(defn ml-enfeature [{:keys [x0 x1] :as page-meta} blocks]
  (let [idx-blocks (map (fn [i blk] (assoc blk :idx i)) (range) blocks)
        block-vec (->> idx-blocks
                       (map (comp
                               #(update % :internal-features dissoc :text-list :lines)
                               #(assoc % :context-features (context-features idx-blocks %))
                               #(assoc % :internal-features (internal-features (assoc % :page-x0 x0
                                                                                        :page-x1 x1))))))
        lookup (mapv core-features block-vec)]
    (map (fn [{:keys [context-features internal-features] :as blk}]
           (-> blk
               (dissoc :context-features :internal-features :idx)
               (assoc :features (-> context-features
                                    (update :blocks-directly-above (partial map lookup))
                                    (update :blocks-directly-below (partial map lookup))
                                    (update :blocks-right (partial map lookup))
                                    (update :blocks-left (partial map lookup))
                                    (merge internal-features)))))
         block-vec)))

;TODO can we ensure that this stays up to date with the graph above ... at compile or test time maybe...
;TODO maybe make each element a vector, where the second element is the default value
;TODO would it make sense to have an exists? slot for each context item so that we can fire on that?
(def features-seq "ensure consistent ordering for the sake of vector construction"
  [[:avg-data-cnt-per-line 0]
   [:num-datapoints 0]
   [:num-sentences 0]
   [:all-caps-ratio 0]
   [:data-ratio 0]
   [:width 0]
   [:bold-ratio 0]
   [:avg-word-cnt-per-line 0]
   [:horizontal-alignment [0 0]]
   [:keyword-start? 0]
   [:num-tokens 0]
   [:num-lines 0]
   [:ends-with-period? 0]
   [:itemized-start? 0]
   [:tokens-per-line 0]
   [:num-english-words 0]
   [:italic-ratio 0]
   [:word-ratio 0]
   [:height 0]
   [:first-token-summary (token-summary "")]

   [:num-blocks-above 0]
   [:num-blocks-directly-above 0]
   [:num-blocks-directly-below 0]
   [:num-blocks-below 0]
   [:num-blocks-right 0]
   [:num-blocks-left 0]

   [:gap-right 0]
   [:gap-left 0]
   [:gap-above 0]
   [:gap-below 0]])

(defn core-vectorize [features]
  (let [feature-order (map first features-seq)]
    (map #(get {false 0 true 1 nil 0} % %)
         (-> features
             (update :horizontal-alignment {:left [1 0] :center [0 1] :float [0 0]})
             (map feature-order)
             flatten))))

(defn context-vectors
  "Ensures consistent features vector length by ensuring a consistent number of
  neighbors.  Adds all zero vectors when context is missing."
  [context-window-size context]
  (->> (repeat context-window-size (flatten (map second features-seq)))
       (concat (map core-vectorize context))
       (take context-window-size)
       (apply concat)))

(defn block-id [{:keys [page-number x0 y0]}]
  (when (and x0 y0)
    (str page-number "_" (int y0) "_" (int x0))))

(defn ml-vectorize [{{:keys [blocks-directly-below blocks-directly-above
                             blocks-left blocks-right] :as features} :features :as block}]
  {:id (block-id block)
   :feature-vec (concat (core-vectorize features)
                        (context-vectors 2 blocks-directly-above)
                        (context-vectors 2 blocks-directly-below)
                        (context-vectors 2 blocks-left)
                        (context-vectors 2 blocks-right))
   :text (:text features)})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Random Forest       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn core-feats [features]
  (->> (dissoc features :blocks-directly-below :blocks-directly-above
               :blocks-left :blocks-right :text :lines :text-list)
       (map (fn [[k v]] [k (get {false 0 true 1 nil 0} v v)]))
       (into {})))

(defn contextual-feats
  "Ensures consistent features vector length by ensuring a consistent number of
  neighbors.  Adds all zero vectors when context is missing."
  [key-prefix core-features window-size context]
  (->> core-features
       (map (fn [[k _]] [k 0]))
       (into {})
       (repeat window-size)
       (concat (map core-feats context))
       (take window-size)
       (map (fn [idx features]
              (into {} (map (fn [[k v]] [(keyword (s/join "-" [key-prefix idx (name k)])) v]) features))) (range))
       (apply merge)))

(defn rand-forest-features [{{:keys [blocks-directly-below blocks-directly-above
                                     blocks-left blocks-right] :as features} :features :as block}]
  (let [core (core-feats features)]
    {:id          (block-id block)
     :feature-vec (-> core
                      (merge (contextual-feats "above" core 2 blocks-directly-above))
                      (merge (contextual-feats "below" core 2 blocks-directly-below))
                      (merge (contextual-feats "left" core 2 blocks-left))
                      (merge (contextual-feats "right" core 2 blocks-right)))
     :text        (:text features)})

  )

;classes
; table-column, table-column-header, text, page-footer, table-footer, label, value, data-point