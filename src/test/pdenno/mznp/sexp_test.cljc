(ns pdenno.mznp.sexp-test
  "Rewrite the mznp parsed structure to 'executable' EDN."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [pdenno.mznp.macros :refer [debug-off]]
            [pdenno.mznp.sexp :refer [rewrite* form-bin-ops*]]))

(deftest simple-sexp
  (testing "Test atomic rewriting tasks."
    (debug-off
     (is (= (rewrite* :mznp/if-then-else-expr "if a then b else c endif") '(if a b c)))
     (is (= (rewrite* :mznp/expr "1") 1))
     (is (= (rewrite* :mznp/expr "1 + x")   '(+ 1 x)))
     (is (= (rewrite* :mznp/expr "(1 + x)") '(+ 1 x)))
     (is (= (rewrite* :mznp/expr "1..8")   '(mznf/mzn-range 1 8)))
     (is (= (rewrite* :mznp/expr "max(x)") '(mznf/mzn-max x)))
     (is (= (rewrite* :mznp/expr "min(x)") '(mznf/mzn-min x)))
     (is (= (rewrite*
             :mznp/expr
             "if (makeRatio < 0.1) then 10 elseif (makeRatio < 0.5) then 6 else 0 endif")
            '(clojure.core/cond (< makeRatio 0.1) 10 (< makeRatio 0.5) 6 :else 0))))))

(deftest reducing-bin-ops
  (testing "Test atomic rewriting tasks."
    (debug-off
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5")
             '(+ (+ 1 (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "x + 2 * 3 + 4 * 5")
             '(+ (+ x (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5" :reduce? true)
             {:pdenno.mznp.sexp/type :MznExpr, :bin-ops [1 \+ 2 \* 3 \+ 4 \* 5]}))
      (is (= (-> (rewrite* :mznp/expr "1 + 2 * 3")) '(+ 1 (* 2 3))))
      ;; Some with primaries
      (is (= (form-bin-ops* "3 * (1 + 2)") '(* 3 (+ 1 2))))
      (is (= (form-bin-ops* "(1 + 2) * 3") '(* (+ 1 2) 3)))
      ;;; Some array references
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (mznf/aref b i))))
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (mznf/aref b i))))
      (is (= (form-bin-ops*  "a[i] * b[i]") '(* (mznf/aref a i) (mznf/aref b i))))
      (is (= (form-bin-ops*  "a[i] * b")    '(* (mznf/aref a i) b))))))

(deftest variable-decl-rewriting
  (testing "Rewriting variable declarations"
    (debug-off
     (is (= (rewrite* :mznp/var-decl-item "int: n = 3")
            '{:name "n", :vartype {:datatype :int}, :mval 3}))
     (is (= (rewrite* :mznp/var-decl-item "set of int: Lines = 1..numLines")
            '{:name "Lines",
              :vartype {:datatype :mzn-set, :base-type :int},
              :mval (mznf/mzn-range 1 numLines)}))
     (is (= (rewrite* :mznp/var-decl-item "array[Lines] of int: LinePenalty")
            '{:name "LinePenalty",
              :vartype {:datatype :mzn-array, :index [Lines], :base-type :int},
              :mval nil}))
     (is (= (rewrite* :mznp/var-decl-item "array [Jobs,Weeks] of var 0..workforce_size: WorkersOnJob")
            '{:name "WorkersOnJob",
              :vartype
              {:datatype :mzn-2d-array,
               :index [Jobs Weeks],
               :base-type (mznf/mzn-range 0 workforce_size)},
              :mval nil,
              :var? true}))
     (is (= (rewrite* :mznp/item "enum ProductType = {A,B,C}")
            {:name "ProductType", :vartype {:datatype :mzn-enum}, :mval ["A" "B" "C"]}
            ;; 2021-05-02 Don't know what this is about. A fix somewhere?
          #_{:name "ProductType", :datatype {:vartype :mzn-enum}, :mval ["A" "B" "C"]})))))

(deftest let-rewriting
  (testing "Rewriting let expressions"
    (debug-off
     (is (= (rewrite* :mznp/let-expr
                      "let { var int: ActualEffort = (LastWeekOfRoute[r] - FirstWeekOfRoute[r] + 1)*TeamsOnRoute[r]*160; } in
                       if (ActualEffort >= RouteEffort[r]) then 0 else (RouteEffort[r] - ActualEffort)*RoutePenalty[r] endif")
            '(clojure.core/let ["ActualEffort" (* (* (+ (- (mznf/aref LastWeekOfRoute r) (mznf/aref FirstWeekOfRoute r)) 1)
                                        (mznf/aref TeamsOnRoute r)) 160)]
               (if (>= ActualEffort (mznf/aref RouteEffort r))
                 0
                 (* (- (mznf/aref RouteEffort r) ActualEffort) (mznf/aref RoutePenalty r)))))))))

(deftest bigger-rewriting-tasks
  (testing "Big rewriting tasks"
    (debug-off
     (is (= (rewrite*
             :mznp/gen-call-expr
             "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")
            '(mznf/sum  [[j Jobs]]   true
                   (if  (= (mznf/aref LineOfJob j) lin) (mznf/aref WorkersOnJob j w1) 0))))
     
     (is (= (rewrite*
             :mznp/constraint-item
             "constraint forall (lin in Lines, w1, w2 in Weeks
                        where w1 < w2                                       /\\
                        forall (j in Jobs) ((LineOfJob[j] = lin) /\\ (WorkersOnJob[j,w1] != 0))  /\\
                        (w2 == max (j in Jobs, w in (w1+1)..numWeeksScheduled)
	                           (if ((LineOfJob[j] == lin) /\\ (WorkersOnJob[j,w] != 0)) then w else 0 endif)))
             ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
              ==
              (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)))")
            '(mznf/forall [[lin Lines] [w1 w2 Weeks]]
                      (and (and (< w1 w2) 
                                            (mznf/forall [[j Jobs]]  true
                                                    (and
                                                     (mznf/assign (mznf/aref LineOfJob j) lin)
                                                     (not= (mznf/aref WorkersOnJob j w1) 0))))
                                    (= w2
                                        (mznf/max
                                         [[j Jobs] [w (mznf/mzn-range (+ w1 1) numWeeksScheduled)]]
                                         true
                                         (if
                                             (and
                                              (= (mznf/aref LineOfJob j) lin)
                                              (not= (mznf/aref WorkersOnJob j w) 0))
                                           w
                                           0))))
                     (=
                      (mznf/sum  [[j Jobs]]  true
                            (if  (= (mznf/aref LineOfJob j) lin)
                              (mznf/aref WorkersOnJob j w1)
                              0))
                      (mznf/sum [[j Jobs]] true
                           (if
                               (= (mznf/aref LineOfJob j) lin)
                             (mznf/aref WorkersOnJob j w2)
                             0)))))))))

(deftest whole-models
  (testing "Rewriting of whole models"
    (debug-off
     (is (= (rewrite* :mznp/model "data/assignment.mzn" :file? true)
            (edn/read-string (slurp "data/output/assignment.edn")))))))
