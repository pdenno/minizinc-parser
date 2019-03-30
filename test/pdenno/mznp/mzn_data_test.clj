(ns pdenno.mznp.mzn-data-test
  "Test creation and spec-ing of MiniZinc data structures."
  (:require [clojure.test :refer :all]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.mzn-data :as mznd]))

(mznd/make-data '{:name "n", :vartype {:datatype :int}, :init 3})
(mznd/make-data '{:name "x", :vartype {:datatype :float}, :init 3.0})
(mznd/make-data '{:name "Workers",
                  :vartype {:datatype :mzn-set, :base-type :int},
                  :init (range-op 1 n)})

(deftest mzn-datastructure
  (testing "that MiniZinc datastructures pass their specs."
    (is (s/valid? :mzn-user/n mzn-user/n))
    (is (s/valid? :mzn-user/x mzn-user/x))
    (is (s/valid? :mzn-user/Workers mzn-user/Workers))))





