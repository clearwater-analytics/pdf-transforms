(ns pdf-transforms.tokens.graphics
  (:require [pdf-transforms.utilities :as utils]))

(defn explode-graphics [graphics]
  (->> graphics
       (keep (fn [{:keys [x0 x1 y0 y1] :as graphic}]
              (cond
                (and (utils/within-x? 2 x0 x1)
                     (not (utils/within-x? 5 y0 y1)))
                (assoc graphic :class :graphic :boundary-axis :x)

                (and (utils/within-x? 2 y0 y1)
                     (not (utils/within-x? 5 x0 x1)))
                (assoc graphic :class :graphic :boundary-axis :y)

                (> (* (- x1 x0) (- y1 y0)) 100)             ;explode box
                [(assoc graphic :class :graphic :boundary-axis :boc)
                 (assoc graphic :x1 x0 :class :graphic :boundary-axis :x)
                 (assoc graphic :x0 x1 :class :graphic :boundary-axis :x)
                 (assoc graphic :y0 y1 :class :graphic :boundary-axis :y)
                 (assoc graphic :y1 y0 :class :graphic :boundary-axis :y)])))
       flatten))
