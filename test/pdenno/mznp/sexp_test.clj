(ns pdenno.mznp.sexp-test
  "Rewrite the mznp parsed structure to 'executable' EDN."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :refer :all]))

(defmacro debug-off [& body]
   `(let [db?# @debugging?
          mznp-db?# @mznp/debugging?]
      (reset! debugging?      false)
      (reset! mznp/debugging? false)
      ~@body
      (reset!      debugging? db?#)
      (reset! mznp/debugging? mznp-db?#)))

(deftest simple-sexp
  (testing "Test atomic rewriting tasks."
    (debug-off
     (is (= (rewrite* ::mznp/expr "1") 1))
     (is (= (rewrite* ::mznp/expr "1 + x") '(+ 1 x)))
     (is (= (rewrite* ::mznp/expr "(1 + x)") '(+ 1 x))))))

(deftest reducing-bin-ops
  (testing "Test atomic rewriting tasks."
    (debug-off
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5")
             '(+ (+ 1 (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "x + 2 * 3 + 4 * 5")
             '(+ (+ x (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5" :reduce? true)
             {:type :MznExpr, :bin-ops [1 \+ 2 \* 3 \+ 4 \* 5]}))
      (is (= (-> (rewrite* ::mznp/expr "1 + 2 * 3")) '(+ 1 (* 2 3))))
      ;; Some with primaries
      (is (= (form-bin-ops* "3 * (1 + 2)") '(* 3 (+ 1 2))))
      (is (= (form-bin-ops* "(1 + 2) * 3") '(* (+ 1 2) 3)))
      ;;; Some array references
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (aacc-op b i))))
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (aacc-op b i))))
      (is (= (form-bin-ops*  "a[i] * b[i]") '(* (aacc-op a i) (aacc-op b i))))
      (is (= (form-bin-ops*  "a[i] * b")    '(* (aacc-op a i) b))))))

(deftest variable-decl-rewriting
  (testing "Rewriting variable declarations"
    (debug-off
     (is (= (rewrite* ::mznp/var-decl-item "int: n = 3")
            '{:name "n", :vartype :int, :init 3}))
     (is (= (rewrite* ::mznp/var-decl-item "set of int: Lines = 1..numLines")
            '{:name "Lines",
              :vartype {:datatype :mzn-set, :base-type :int},
              :init (range-op 1 numLines)}))
     (is (= (rewrite* ::mznp/var-decl-item "array[Lines] of int: LinePenalty")
            '{:name "LinePenalty",
              :vartype {:datatype :mzn-array, :index [Lines], :base-type :int},
              :init nil}))
     (is (= (rewrite* ::mznp/var-decl-item "array [Jobs,Weeks] of var 0..workforce_size: WorkersOnJob")
            '{:name "WorkersOnJob",
              :vartype
              {:datatype :mzn-array,
               :index [Jobs Weeks],
               :base-type (range-op 0 workforce_size)},
              :init nil,
              :var? true})))))

(deftest bigger-rewriting-tasks
  (testing "Big rewriting tasks"
    (debug-off
     (is (= (rewrite*
             ::mznp/gen-call-expr
             "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")
            '(sum  [[j Jobs]]   true
                   (if  (== (aacc-op LineOfJob j) lin) (aacc-op WorkersOnJob j w1) 0))))
     
     (is (= (rewrite*
             ::mznp/constraint-item
             "constraint forall (lin in Lines, w1, w2 in Weeks
                        where w1 < w2                                       /\\
                        forall (j in Jobs) ((LineOfJob[j] = lin) /\\ (WorkersOnJob[j,w1] != 0))  /\\
                        (w2 == max (j in Jobs, w in (w1+1)..numWeeksScheduled)
	                           (if ((LineOfJob[j] == lin) /\\ (WorkersOnJob[j,w] != 0)) then w else 0 endif)))
             ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
              ==
              (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)))")
            '(forall [[lin Lines] [w1 w2 Weeks]]
                      (and-op (and-op (< w1 w2) 
                                            (forall [[j Jobs]]  true
                                                    (and-op
                                                     (assign (aacc-op LineOfJob j) lin)
                                                     (not= (aacc-op WorkersOnJob j w1) 0))))
                                    (== w2
                                        (max
                                         [[j Jobs] [w (range-op (+ w1 1) numWeeksScheduled)]]
                                         
                                         true
                                         (if
                                             (and-op
                                              (== (aacc-op LineOfJob j) lin)
                                              (not= (aacc-op WorkersOnJob j w) 0))
                                           w
                                           0))))
                     (==
                      (sum  [[j Jobs]]  true
                            (if  (== (aacc-op LineOfJob j) lin)
                              (aacc-op WorkersOnJob j w1)
                              0))
                      (sum [[j Jobs]] true
                           (if
                               (== (aacc-op LineOfJob j) lin)
                             (aacc-op WorkersOnJob j w2)
                             0)))))))))

;;; (rewrite* ::mznp/model "data/penalty.mzn" :file? true)
