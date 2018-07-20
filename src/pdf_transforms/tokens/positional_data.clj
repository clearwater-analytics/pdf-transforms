(ns pdf-transforms.tokens.positional-data
  "Functions for converting a stream of character positions into
  a sequence of pages, where a page is a sequence of word positions."
  (:require [pdf-transforms.utilities :as utils]
            [clojure.string :as s]))

(defn- paginate [char-positionals]
  (->> char-positionals
       (group-by :page-number)
       (sort-by key)
       (map second)))

(defn duplicate-char? [{x0 :x y0 :y text0 :text}
                       {:keys [x y text]}]
  (and
    (= text0 text)
    (< (Math/abs (- y y0)) 2)
    (neg? (- x x0))))

(defn same-word? [{x0 :x w0 :width y0 :y font-size0 :font-size cg :char-gap}
                  {:keys [x y font-size]}]
  (and
    ;(= font-size0 font-size)
    (<= (dec (Math/abs (- x (+ w0 x0)))) (or cg 0))
    (< (Math/abs (- y0 y)) 4)))

;is the superscript on the right of the modified term
(defn super-scripted-right? [{y0 :y font-size0 :font-size :as word}
                              {:keys [y font-size] :as char}]
  (and
    (not (utils/gap? word char))
    (< font-size font-size0)
    (pos? (- y0 y 1))))

(defn super-scripted-left? [words {:keys [y height font-size horizontal-bar?]}]
  (let [{ss-y :y ss-size :font-size ss-text :text
         ss-hbar? :horizontal-bar? :as ss-word} (peek words)]
    (and
      (not horizontal-bar?)
      (not ss-hbar?)
      (> font-size ss-size)
      (utils/between? ss-y (- y height height) (+ y height))
      (if-let [p-word (peek (pop words))]
        (or (utils/gap? p-word ss-word) (utils/new-line? p-word ss-word))
        true)
      (< (count ss-text) 4))))


(defn super-script? [{y0 :y font-size0 :font-size supe? :superscript?}
                       {:keys [y font-size]}]
  (and
    supe?
    (> font-size font-size0)
    (pos? (- y y0 1))))

(def whitespace #"\s*")

(defn build-tokens [characters]
  (reduce (fn [words {:keys [x width text] :as char}]
            (let [{wx :x wt :text cg :char-gap ellipsis :ellipsis wwidth :width :as word} (peek words)]
              (cond
                (or (duplicate-char? word char)
                    (re-matches whitespace text)) words
                (and ellipsis
                     (or
                       (utils/new-line? word char)
                       (not (re-matches #"\.+" text)))) (cond-> (pop words)
                                                                (seq wt) (conj (dissoc word :ellipsis))
                                                                true (conj ellipsis char))
                (super-scripted-right? word char) (conj words (assoc char :superscript? true))
                (super-scripted-left? words char) (conj (pop words)
                                                        (assoc word :superscript? true)
                                                        char)
                (super-script? word char) (conj words char)
                (same-word? word char) (conj (pop words)
                                             (cond
                                               ellipsis (-> word
                                                            (update-in [:ellipsis :width] (partial + width))
                                                            (update-in [:ellipsis :text] #(str % "."))) ;we know from other cond that char is a .
                                               (and (s/ends-with? wt ".")
                                                    (re-matches #"\.+" text)) (assoc word :ellipsis (assoc char :ellipsis? true
                                                                                                                :width (* 2.0 width) ;we know the current char is a dot, so use its width as the standard dot width
                                                                                                                :x (- x width)
                                                                                                                :text "..")
                                                                                          :text (subs wt 0 (dec (count wt)))
                                                                                          :width (- wwidth width))
                                               :else (assoc word :width (- (+ x width) wx)
                                                                 :char-gap (/ (+ (or cg 0) (Math/abs (- x (+ wx wwidth))))
                                                                              (if (<= (count (:text word)) 2) 1 2))
                                                                 :text (str wt text))))
                ellipsis (conj (pop words) (-> word         ;due to (ellipsis and NOT dot) condition above, IF ellipsis THEN dot
                                               (update :ellipsis #(assoc % :width (- (+ x width) (:x %))))
                                               (update-in [:ellipsis :text] #(str % "."))))
                (and (re-matches #".*\." wt)                  ;not part of the same word, but still dots
                     (= text ".")
                     (not (utils/new-line? word char)))
                (conj (pop words) (assoc word :ellipsis (assoc char :ellipsis? true
                                                                    :width (* 2.0 width) ;we know the current char is a dot, so use its width as the standard dot width
                                                                    :x (- x width)
                                                                    :text "..")
                                              :text (subs wt 0 (dec (count wt)))
                                              :width (- wwidth width)))
                :else (conj words char))))
          [(first characters)]
          (rest characters)))

(defn page->token-stream [page]
  (->> page
       utils/create-lines
       (apply concat)
       (drop-while #(re-matches whitespace (:text %)))
       build-tokens
       rseq
       (reduce (fn [words {:keys [y font-size] :as curr-word}]
                 (let [{wy :y wfont-size :font-size ss? :superscript?} (peek words)]
                   (conj words (if (and ss?
                                        (utils/within-x? 1 wy y)
                                        (= font-size wfont-size))
                                 (assoc curr-word :superscript? true)
                                 curr-word)))) [])
       rseq))

(defn build-id [{:keys [page-number x y]}]
  (str page-number "_" (int y) "_" (int x)))

(defn text-positions->pages-of-tokens [text-positions]
  (->> text-positions
       (map #(assoc % :id (build-id %)))
       paginate
       (map page->token-stream)
       (filter first)))
