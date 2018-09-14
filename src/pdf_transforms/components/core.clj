(ns pdf-transforms.components.core
  "Higher level namespace which contains the function for composing blocks on a page into components."
  (:require [pdf-transforms.components.text :as txt]
            [pdf-transforms.components.tables :as tbl]
            [pdf-transforms.common :as cmn]
            [pdf-transforms.utilities :as utils]))

(defn- blocks->components [page fncs blocks]
  (->> fncs
       (reduce (fn [{:keys [components raw-blocks] :as state} fnc]
                 (let [comps (fnc raw-blocks page)]
                   (assoc state :components (apply conj components comps)
                                :raw-blocks (reduce #(remove (partial cmn/within? %2) %1) raw-blocks comps))))
               {:components [] :raw-blocks blocks})
       ((fn [{:keys [components raw-blocks]}]
          (concat components (map txt/->text raw-blocks))))))

;TODO Does this really need page and blocks as arguments or can it be rewritten to just use blocks?
(defn ->components
  "Converts a page and its blocks into "
  [page blocks]
  (->> blocks
       (blocks->components (remove :horizontal-bar? page) [tbl/->standard-tables
                                                           txt/->labels-with-data])
       cmn/sort-blocks))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;          NEW STUFF           ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn horizontally-near? [{x0 :x1} {x1 :x0}]
  (< (- x1 x0) 150))

(defn compatible-classes? [blocks {:keys [class]}]
  (let [classes (mapv :class blocks)]
    (or
      ;(or (= :key (last classes)) (= :key-column (last classes))) ;key to left
      (and (or (= :table-cell class) (= :table-column class)) ;data to the right
           (or (= :table-cell (last classes)) (= :table-column (last classes)))) ;data to the left

      )))

(defn filter-candidates [block other-blocks]
  (:candidates
    (reduce (fn [{:keys [candidates] :as box} blk]
              (if (and (horizontally-near? (last candidates) blk)
                       (compatible-classes? candidates blk))
                (update box :candidates conj blk)
                (reduced box)))
            (assoc block :candidates [block])
            (filter (comp (partial = #{:right}) (partial cmn/relative-to block)) other-blocks))))



(defn cluster-adjacent [{{:keys [num-components-left num-components-right]} :features :as block}
                        other-blocks]
  (if (zero? (+ num-components-left num-components-right))
    [block]
    (filter-candidates block other-blocks)))


(def format-clusters
  (comp
    #(select-keys % [:x0 :x1 :y0 :y1 :page-number :tokens])
    (partial reduce (fn [res {:keys [tokens] :as seg}]
                      (-> res
                          (utils/expand-bounds seg)
                          (update :tokens #(concat % tokens)))))))


(defn compose-clusters [blocks]
  (loop [clusters []
         remaining blocks]
    (if-not (seq remaining)
      clusters
      (let [cluster (cluster-adjacent (first remaining) (rest remaining))]
        (recur (conj clusters (format-clusters cluster)) (remove (into #{} cluster) remaining))))))









#_(->> blocks
     (filter (fn [{{:keys [num-components-left num-components-right]} :features class :class}]
               (and
                 (pos? (+ num-components-left num-components-right)))))
     (utils/partition-when (fn [a b] (not= #{:right} (cmn/relative-to a b)))) ;grouped by vertical alignment
     (map (partial partition 2 1))                 ;make pairs of side by side blocks
     (map (fn [pairs] (reduce
                        (fn [{prev :prev {:keys [class x1] :as a} :this :as state}
                             {x0 :x0 bclass :class :as b}]
                          (cond
                            (> (- x0 x1) 200) (assoc state :this b) ;ignore the previous, too far away
                            (and (or (= :data-point bclass)
                                     (= :data-column bclass))
                                 (or (= :data-column class)
                                     (= :data-point class))) (update state :this #(utils/expand-bounds % b))
                            (= :key bclass)

                            )

                          )

                        {:prev [] :this (first pairs)} (rest pairs))))


     #_(map (fn [[{:keys [class x1] :as a} {x0 :x0 bclass :class :as b}]]
              (let [l-type (cond
                             (and (= :key class))

                             )


                    #_(cond
                        (> (- x0 x1) 200) nil
                        (and )
                        (and (or (= :data-point bclass)
                                 (= :data-column bclass))
                             (or (= :data-column class)
                                 (= :data-point class))) :data-column
                        (and (or (= :data-point class)
                                 (= :data-point bclass))) nil ;ignore if data is in label position
                        (= :paragraph class) nil       ;ignore if paragraph on left
                        )]
                (assoc (utils/expand-bounds a b) :class l-type))))
     (filter :class)

     ;(map (partial reduce utils/expand-bounds))
     )