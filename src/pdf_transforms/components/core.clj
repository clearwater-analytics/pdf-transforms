(ns pdf-transforms.components.core
  "Higher level namespace which contains the function for composing blocks on a page into components."
  (:require [pdf-transforms.components.text :as txt]
            [pdf-transforms.components.tables :as tbl]
            [pdf-transforms.common :as cmn]))

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