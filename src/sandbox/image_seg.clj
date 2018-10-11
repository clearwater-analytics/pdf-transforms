(ns sandbox.image-seg)

(defn round-out
  "Rounds n to the nearest multiple of mult-of in an inclusive manner.
  That is, if up? then the function will round up to cover the block."
  [n mult-of dir]
  (let [div (/ n mult-of)
        up? (= dir :up)
        low-end (* (Math/floor div) mult-of)
        high-end (* (Math/ceil div) mult-of)]
    (if up?
      (if (> (- n low-end) (/ mult-of 2.0))                 ;the data overlaps the block by some minimal amount
        high-end
        low-end)
      (if (> (- high-end n) (/ mult-of 2.0))                 ;the data overlaps the block by some minimal amount
        low-end
        high-end))))


(defn segment->chunks [step {:keys [x0 x1 y0 y1]}]
  (for [x (range (round-out x0 step :down) (round-out x1 step :up) step)
        y (range (round-out y0 step :down) (round-out y1 step :up) step)]
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

