(ns pdf-transforms.blocks.core
  "Functions for grouping words into semantic chunks, where whitespace and
  font changes are used to infer boundaries."
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]
            [clojure.set :as s]))

(defn vertically-near? [{:keys [y1 height tokens]}
                        {by0 :y0 b-height :height b-tokens :tokens}]
  (< (- by0 y1) (* (if (and (seq (s/intersection (set (map :font tokens)) (set (map :font b-tokens))))
                            (seq (s/intersection (set (map :font-size tokens)) (set (map :font-size b-tokens)))))
                     0.75
                     0.25)
                   (min height b-height))))

(defn y-boundary-between? [graphics {ay1 :y1 ax0 :x0 ax1 :x1} {by0 :y0}]
  (some
    (fn [{:keys [x0 x1 y0 y1 boundary-axis]}]
      (and (= :y boundary-axis)                   ;boundary separates along the y axis
           (>= (inc y0) ay1)                      ;boundary is below top segment
           (>= (inc by0) y1)                      ;boundary is above bottom segment
           (>= (- (min ax1 x1) (max ax0 x0)) 4.0)))                              ;sufficient overlap in horizontal direction
    graphics))


(defn filter-candidates [segment other-segments graphics blocks]
  (:candidates
    (reduce (fn [{:keys [candidates] :as box} seg]
              (if (and (= #{:below} (cmn/forgiving-relative-to box seg))
                       (vertically-near? (last candidates) seg)
                       (not (y-boundary-between? graphics box seg)))

                ;now check if adding this segment would lead to merge side-by-side segments
                (let [{cands :candidates :as hypothesis} (-> box (utils/expand-bounds seg) (update :candidates conj seg))]
                  (if (some (fn [seg-x] (and (empty? (cmn/forgiving-relative-to hypothesis seg-x)) ;within block
                                             (not (some (partial = seg-x) cands)))) ;but not a candidate
                            (concat other-segments blocks))
                    (reduced box)
                    hypothesis))
                box))
            (assoc segment :candidates [segment])
            other-segments)))


(defn group-vertically [segment other-segments other-blocks graphics]
  (let [calc-diff #(- (:y0 (second %)) (:y1 (first %)))
        pairs (->> other-blocks
                   (filter-candidates segment other-segments graphics)
                   (partition 2 1))]
    (case (count pairs)
      0 [segment]
      1 (first pairs)
      (:block (reduce (fn [{:keys [last-diff block]} pair]  ;group by magnitude of separation
                        (let [diff (calc-diff pair)]
                          (cond
                            (< diff 0.001)                  ;deal with division by 0
                            {:last-diff diff :block (conj block (second pair))}
                            (< (Math/abs (- 1.0 (/ last-diff diff))) 0.3)
                            {:last-diff diff :block (conj block (second pair))}
                            (> diff last-diff)
                            (reduced {:block block})
                            :else (reduced {:block (butlast block)}))))
                      {:last-diff (calc-diff (first pairs))
                       :block     (into [] (first pairs))}
                      (rest pairs))))))


(def format-block
  (comp
    #(select-keys % [:x0 :x1 :y0 :y1 :page-number :tokens])
    (partial reduce (fn [res {:keys [tokens] :as seg}]
                      (-> res
                          (utils/expand-bounds seg)
                          (update :tokens #(concat % tokens)))))))


(defn compose-blocks [segments graphics]
  (loop [blocks []
         remaining segments]
    (if-not (seq remaining)
      blocks
      (let [block (group-vertically (first remaining) (rest remaining) blocks graphics)]
        (recur (conj blocks (format-block block)) (remove (into #{} block) remaining))))))
