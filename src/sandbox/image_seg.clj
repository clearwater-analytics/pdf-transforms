(ns sandbox.image-seg)

;TODO maybe try this with

(defn round
  "Rounds n to the nearest multiple of mult-of"
  [n mult-of]
  (let [div (/ n mult-of)
        low-end (* (Math/floor div) mult-of)]
    (if (< (- n low-end) (/ mult-of 2.0))
      low-end
      (* (Math/ceil div) mult-of))))


;;;;
(defn segment->chunks [step {:keys [x0 x1 y0 y1]}]
  (for [x (range (round x0 step) (round x1 step) step)
        y (range (round y0 step) (round y1 step) step)]
    {:x0 x :x1 (+ x step) :y0 y :y1 (+ y step)
     :id (str (int x) "-" (int y))}))



;if a block is taken up by text, add it
;'value' of block is determined by majority occupant
(defn segments->chunks [step segments]
  (let [max-dim 800                ;empirical
        page-num (some :page-number segments)
        filled-chunk? (->> segments (mapcat (partial segment->chunks step)) (map :id) (into #{}))]
    (for [x (range 0 max-dim step)
          y (range 0 max-dim step)]
      {:x0 x :x1 (+ x step) :y0 y :y1 (+ y step) :page-number page-num
       :type (when (filled-chunk? (str x "-" y)) :table)})))

