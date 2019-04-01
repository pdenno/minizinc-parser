(ns pdenno.mznp.mzn-data-test
  "Test creation and spec-ing of MiniZinc data structures."
  (:require [clojure.test :refer :all]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.mzn-fns :as mznf]
            [pdenno.mznp.mzn-data :as mznd]))

(def test-model-1 (mznd/process-model "data/assignment.mzn"))

(deftest mzn-data-structure
  (testing "that MiniZinc data structures pass their specs."
    (mznd/intern-data '{:name "n", :vartype {:datatype :int}, :value 3})
    (mznd/intern-data '{:name "x", :vartype {:datatype :float}, :value 3.0})
    (mznd/intern-data '{:name "Workers",
                        :vartype {:datatype :mzn-set, :base-type :int},
                        :value (mznf/range 1 n)})
    (mznd/intern-data '{:name "Tasks",
                        :vartype {:datatype :mzn-set, :base-type :int},
                        :value (mznf/range 1 n)})
    (mznd/intern-data '{:name "DoesTask",
                        :vartype {:datatype :mzn-array, :index [Workers], :base-type Tasks}, ; Hey, cool!
                        :value [3 2 1],
                        :var? true}) 
    (mznd/intern-data '{:name "Cost", 
                        :vartype
                        {:datatype :mzn-2d-array, :index [Workers Tasks], :base-type :int},
                        :value [[10 20 13] [22 11 31] [14 20 18]]})
    (is (s/valid? :mzn-user/n mzn-user/n))
    (is (s/valid? :mzn-user/x mzn-user/x))
    (is (s/valid? :mzn-user/Workers mzn-user/Workers))
    (is (s/valid? :mzn-user/Tasks mzn-user/Tasks))
    (is (s/valid? :mzn-user/DoesTask mzn-user/DoesTask))
    (is (mznd/populated? mzn-user/n))
    (is (mznd/populated? mzn-user/x))
    (is (mznd/populated? mzn-user/Tasks))
    (is (mznd/populated? mzn-user/Tasks))))

(def define-path-model
  '{:core
    {:var-decls
     {:x           {:name "x",           :vartype {:datatype :int}, :value         3},
      :v1          {:name "v1",          :vartype {:datatype :int}, :value (my-fn x)},
      :v2          {:name "v2",          :vartype {:datatype :int}, :value (my-fn v1)},
      :v3-ng       {:name "v3-ng",       :vartype {:datatype :int}, :value (my-fn non-existant)},
      :v3          {:name "v3",          :vartype {:datatype :int}, :value (my-fn v2)},
      :v4-not-able {:name "v4-not-able", :vartype {:datatype :int}, :value (my-fn v3-ng)},
      :v4          {:name "v4",          :vartype {:datatype :int}, :value (my-fn v3)},
      :my-var      {:name "v4",          :vartype {:datatype :int}, :value (my-difficult-fn v3) :var? true}}}})

(deftest var-dependencies
  (testing "variable dependency ordering"
    (is (= (mznd/var-dependency-order define-path-model) [:x :v1 :v2 :v3 :v4]))))


        





