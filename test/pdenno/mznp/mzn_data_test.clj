(ns pdenno.mznp.mzn-data-test
  "Test creation and spec-ing of MiniZinc data structures."
  (:require [clojure.test :refer :all]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.mzn-data :as mznd]))


(deftest mzn-datastructure
  (testing "that MiniZinc data structures pass their specs."
    (mznd/make-data '{:name "n", :vartype {:datatype :int}, :value 3})
    (mznd/make-data '{:name "x", :vartype {:datatype :float}, :value 3.0})
    (mznd/make-data '{:name "Workers",
                      :vartype {:datatype :mzn-set, :base-type :int},
                      :value (range-op 1 n)})
    (mznd/make-data '{:name "Tasks",
                      :vartype {:datatype :mzn-set, :base-type :int},
                  :value (range-op 1 n)})
    #_(mznd/make-data '{:name "DoesTask",
                        :vartype {:datatype :mzn-array, :index [Workers], :base-type Tasks}, ; Hey, cool!
                        :value [3 2 1],
                        :var? true}) 
    #_(mznd/make-data '{:name "Cost", ; POD fix this. 
                        :vartype
                        {:datatype :mzn-2d-array, :index [Workers Tasks], :base-type :int},
                        :value [[10 20 13] [22 11 31] [14 20 18]]})
    (is (s/valid? :mzn-user/n mzn-user/n))
    (is (s/valid? :mzn-user/x mzn-user/x))
    #_(is (s/valid? :mzn-user/Workers mzn-user/Workers))
    #_(is (s/valid? :mzn-user/Tasks mzn-user/Tasks))
    #_(is (mznd/variable? mzn-user/DoesTask))
    #_(is (mznd/populated? mzn-user/n))
    #_(is (mznd/populated? mzn-user/x))
    #_(is (mznd/populated? mzn-user/Tasks))
    #_(is (mznd/populated? mzn-user/Tasks))))
        





