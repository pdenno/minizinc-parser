(ns pdenno.mznp.mzn-data-test
  "Test creation and spec-ing of MiniZinc data structures."
  (:require [clojure.test :refer :all]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.mzn-fns :as mznf]
            [pdenno.mznp.mzn-data :as mznd]))

(deftest mzn-data-structure
  (testing "that MiniZinc data structures pass their specs."
    (mznd/process-model! "data/assignment.mzn")
    (is (not (nil? mzn-user/n)))
    (is (not (nil? mzn-user/Tasks)))
    (is (not (nil? mzn-user/Workers)))
    (is (not (nil? mzn-user/Cost)))
    (is (s/valid? :mzn-user/n       mzn-user/n))
    (is (s/valid? :mzn-user/Tasks   mzn-user/Tasks))
    (is (s/valid? :mzn-user/Workers mzn-user/Workers))
    (is (s/valid? :mzn-user/Cost    mzn-user/Cost))
    (is (s/valid? :mzn-user/Cost      [[10 20 13] [22 11 31] [14 20 13]]))
    (is (not (s/valid? :mzn-user/Cost [[10 20 13] [22 11 31] [14 20 1.3]]))) ; not everything an int
    (is (not (s/valid? :mzn-user/Cost [[10 20]    [22 11     [14 20]]])))    ; subvectors too small
    (is (not (s/valid? :mzn-user/Cost [[10 20 13] [22 11 31]])))             ; too few subvectors
    (is (s/valid? :mzn-user/DoesTask [3 2 1]))
    (is (not (s/valid? :mzn-user/DoesTask [4 2 1])))   ; 4 not in index set Tasks. 
    (is (s/valid? :mzn-user/Tasks-elem 3))
    (is (not (s/valid? :mzn-user/Tasks-elem 4)))))  ; 4 not in index set Tasks.


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


        





