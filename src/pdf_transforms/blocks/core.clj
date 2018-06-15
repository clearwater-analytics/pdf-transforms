(ns pdf-transforms.blocks.core
  "Functions for grouping words into semantic chunks, where whitespace and
  font changes are used to infer boundaries."
  (:require [pdf-transforms.utilities :as utils]
            [pdf-transforms.common :as cmn]))

(defn vertically-near? [{:keys [y1 font font-size height]}
                        {by0 :y0 b-font :font b-font-size :font-size b-height :height}]
  (< (- by0 y1) (* (if (and (= font b-font) (= font-size b-font-size))
                     2.0
                     1.0)
                   (min height b-height))))


(defn filter-candidates [segment other-segments blocks]
  (:candidates
    (reduce (fn [{:keys [candidates] :as box} seg]
              (if (and (= #{:below} (cmn/relative-to box seg))
                       (vertically-near? (last candidates) seg))
                (let [{cands :candidates :as hypothesis} (-> box (utils/expand-bounds seg) (update :candidates conj seg))]
                  (if (some (fn [seg-x] (and (empty? (cmn/relative-to hypothesis seg-x)) ;within block
                                             (not (some (partial = seg-x) cands)))) ;but not a candidate
                            (concat other-segments blocks))
                    (reduced box)
                    hypothesis))
                box))
            (assoc segment :candidates [segment])
            other-segments)))


(defn group-vertically [segment other-segments other-blocks]
  (let [calc-diff #(- (:y0 (second %)) (:y1 (first %)))
        pairs (->> other-blocks
                   (filter-candidates segment other-segments)
                   (partition 2 1))]
    (case (count pairs)
      0 [segment]
      1 (first pairs)
      (:block (reduce (fn [{:keys [last-diff block]} pair]  ;group by magnitude of separation
                        (let [diff (calc-diff pair)]
                          (cond
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


(defn compose-blocks [segments]
  (loop [blocks []
         remaining segments]
    (if-not (seq remaining)
      blocks
      (let [block (group-vertically (first remaining) (rest remaining) blocks)]
        (recur (conj blocks (format-block block)) (remove (into #{} block) remaining))))))
