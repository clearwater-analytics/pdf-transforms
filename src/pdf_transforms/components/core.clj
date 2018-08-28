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

;TODO give all blocks an id (page specific, probably just its index), a component is a set of blocks, which we can represent as a set of block ids
;  This way, is block in a component? is easy and we can have all of the context for a page while also having
;partitioned things into components.

;build the component of which this block is a member
;returns a components and unclassified blocks
(defn big-mama-hug [& x] x #_[{:class class
                     {:keys [neighbors-right]} :features
                     :as block}
                    blocks]
  #_(cond
    (and (#{:key :key-column} class)



         )




    ))

(defn new-components [blocks]
  (loop [components []
         blocks-left blocks]
    (if (empty? blocks-left)
      components
      (let [{:keys [component leftover]} (big-mama-hug (first blocks-left) (rest blocks-left))]
        (recur (conj components component) leftover)))))















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