(ns pdf-transforms.spec
  "Contains the spec definitions for pdf-transforms data structures."
  #?(:clj (:require [clojure.spec.alpha :as s]
                     [clojure.spec.gen.alpha :as gen]
                     [clojure.pprint :as pp])
     :cljs (:require [cljs.spec :as s]
             [cljs.spec.gen.alpha :as gen]
             [cljs.pprint :as pp])))


(defn coordinate? [x]
  (and (number? x) (>= x 0) (< x 1000)))

;page-num_x-coord-of-left_y-coord-of-bottom
(s/def ::id (s/and string? (partial re-matches #"\d{1,3}_\d{1,3}_\d{1,3}")))
(s/def ::text string?)
(s/def ::bold? boolean?)
(s/def ::italic? boolean?)
(s/def ::f-size number?)
(s/def ::x coordinate?)
(s/def ::y coordinate?)
(s/def ::height number?)
(s/def ::width number?)
(s/def ::page-number number?)

(s/def ::x0 coordinate?)
(s/def ::x1 coordinate?)
(s/def ::y0 coordinate?)
(s/def ::y1 coordinate?)

(s/def ::boundaries
  (s/and (s/keys :req-un [::x0 ::x1 ::y0 ::y1 ::page-number])
         (fn [{:keys [x0 x1 y0 y1]}]
           (and (< y0 y1) (< x0 x1)))))

(s/def ::raw-token
  (s/keys :req-un [::text ::bold? ::f-size ::italic? ::x ::y ::height ::width ::page-number]))

(s/def ::token
  (s/keys :req-un [::text ::f-size ::id ::height ::width ::x ::y ::page-number]
          :opt-un [::bold? ::italic? ::superscript?]))

(s/def ::content (s/coll-of ::raw-token))

(s/def ::block
  (s/merge ::boundaries (s/keys :req-un [::features ::content])))

;;;;;;;;;;;;;;;;;;;;;;
;;   Components     ;;
;;;;;;;;;;;;;;;;;;;;;;
(s/def :table/type #{:table "table"})
(s/def :table/vals (s/coll-of (s/coll-of (s/coll-of ::token))))
(s/def :component/table
  (s/merge ::boundaries (s/keys :req-un [:table/type :table/vals])))

(s/def :text/type #{:text "text"})
(s/def :text/vals (s/coll-of (s/coll-of ::token)))
(s/def :component/text
  (s/merge ::boundaries (s/keys :req-un [:text/type :text/vals])))

(s/def :title/type #{:title "title"})
(s/def :title/vals (s/coll-of (s/coll-of ::token)))
(s/def :component/title
  (s/merge ::boundaries (s/keys :req-un [:title/type :title/vals])))


(s/def ::components (s/coll-of (s/or :table :component/table
                                    :text :component/text
                                    :title :component/title)))

(def components? (partial s/explain ::components))
;(gen/sample (s/gen ::components))

;; Generating samples
;(-> ::component s/gen gen/sample pp/pprint)
;(-> ::xrange s/gen gen/sample pp/pprint)
