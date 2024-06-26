(ns mznp.mzn-data-test
  "Test creation and spec-ing of MiniZinc data structures."
  (:require [clojure.test :refer [deftest is testing]]
            [spec-sha.spec :as s]
            [mznp.mzn-data :as mznd]
            [mznp.mzn-user :as mznu]))

(deftest mzn-data-structure
  (testing "that MiniZinc data structures pass their specs."
    (mznd/process-model! "data/assignment.mzn") ; Run for side-effects.
    (is (not (nil? (mznd/user-eval 'n))))
    (is (not (nil? (mznd/user-eval 'Tasks))))
    (is (not (nil? (mznd/user-eval 'Workers))))
    (is (not (nil? (mznd/user-eval 'Cost))))
    (is (s/valid? ::mznu/n       (mznd/user-eval 'n)))
    (is (s/valid? ::mznu/Tasks   (mznd/user-eval 'Tasks)))
    (is (s/valid? ::mznu/Workers (mznd/user-eval 'Workers)))
    (is (s/valid? ::mznu/Cost    (mznd/user-eval 'Cost)))
    (is (s/valid? ::mznu/Cost      [[10 20 13] [22 11 31] [14 20 13]]))
    (is (not (s/valid? ::mznu/Cost [[10 20 13] [22 11 31] [14 20 1.3]]))) ; not everything an int
    (is (not (s/valid? ::mznu/Cost [[10 20]    [22 11     [14 20]]])))    ; subvectors too small
    (is (not (s/valid? ::mznu/Cost [[10 20 13] [22 11 31]])))             ; too few subvectors
    (is (s/valid? ::mznu/DoesTask [3 2 1]))
    (is (not (s/valid? ::mznu/DoesTask [4 2 1])))   ; 4 not in index set Tasks.
    (is (s/valid?      ::mznu/DoesTask-prop 3))
    (is (not (s/valid? ::mznu/DoesTask-prop 4)))))  ; 4 not in index set Tasks.

(def define-path-model
  '{:core
    {:var-decls
     {:x           {:name "x",           :vartype {:datatype :int}, :val         3},
      :v1          {:name "v1",          :vartype {:datatype :int}, :val (my-fn x)},
      :v2          {:name "v2",          :vartype {:datatype :int}, :val (my-fn v1)},
      :v3-ng       {:name "v3-ng",       :vartype {:datatype :int}, :val (my-fn non-existant)},
      :v3          {:name "v3",          :vartype {:datatype :int}, :val (my-fn v2)},
      :v4-not-able {:name "v4-not-able", :vartype {:datatype :int}, :val (my-fn v3-ng)},
      :v4          {:name "v4",          :vartype {:datatype :int}, :val (my-fn v3)},
      :my-var      {:name "v4",          :vartype {:datatype :int}, :val (my-difficult-fn v3) :var? true}}}})

;;; 2021-10-28 This one needs work. It is returning [:v2 :v3-ng :v1 :v4-not-able :v4 :v3 :x]
#_(deftest data-dependencies
  (testing "variable dependency ordering"
    (is (= (mznd/data-dependency-order define-path-model) [:x :v1 :v2 :v3 :v4]))))
