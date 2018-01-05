(ns pdf-transforms.core-test
  (:require
    [pdf-transforms.core :as pdf]
    [clojure.test :refer :all]
    [clojure.java.io :as io]))


(deftest basic-blocks-check
  (testing "Table parsed correctly"
    (let [blocks (pdf/transform (io/resource "sample_1.pdf") {:format :blocks})]
      (is (= 14 (count blocks)))
      (is (-> blocks last :class (= :page-footer)))
      (is (some (comp (partial re-matches #"[*]\s+Percent.*")
                      (partial clojure.string/join " ")
                      (partial map :text)
                      :content) blocks) "join footnote block"))))


(deftest basic-components-check
  (testing "Table parsed correctly"
    (let [components (pdf/transform (io/resource "sample_1.pdf"))
          table (first (filter (comp #(= :table %) :type) components))]
      (is table "Find the table")
      (is (-> table :vals first count (= 3)) "Correct column count")
      (is (-> table :vals count (= 6)) "Correct row count")
      (is (= 9 (count components)) "Correct number of components found"))))