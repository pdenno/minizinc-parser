(ns pdenno.mznp.mzn-fns
  "Macros that 'implement' (for execution and explanation) MiniZinc data structures,
   generator call expressions, etc."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :as sexp]))

(def xxx 1.234)
(def Workers 3) ; This should be a set, but for now...
(def Tasks 3)   ; This should be a set, but for now...

(def foo
  '{:name "cost",
    :vartype
    {:datatype :mzn-array, :index [Workers Tasks], :base-type :int},
    :init [[10 20 13] [22 11 31] [14 20 18]]})

(def not-foo
  '{:name "cost",
    :vartype
    {:datatype :mzn-array, :index [Workers Tasks], :base-type :int},
    :init [[10 20 13] [22 xxx 31] [14 20 18]]})

(s/def ::int-vec-3 (s/coll-of integer? :kind vector? :count 3))
(s/def ::foo (s/coll-of ::int-vec-3 :kind vector? :count 3)) ; Need to specify sizes, so generate ::int-vec. 

;;; POD It would be nice to say *what symbol* is unresolved. In tracking this down, 
;;; of course, I will have to watch for cycles. 
(defn user-eval
  "Do clojure eval in namespace mzn-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [x]
  (eval x))

(defn make-data-set
  "Make a clojure object representing a MiniZinc set."
  [var-decl]

;;; (-> var-decl :vartype :datatype #{:int :float :mzn-set :mzn-array :mzn-2d-array})
  

;;; POD Of course, will need an "mzn-user" namespace...
(defn make-data-2darray
  "Make a clojure object representing the argument MiniZinc 2D array."
  [var-decl]
  (let [data (-> var-decl :init user-eval)
        sizes (mapv user-eval (-> var-decl :vartype :index))] ; This needs to count, not as above.
    (s/def (-> var-decl :name keyword) (s/coll-of ::int-vec-3)))) ; NYI
      
    
    
        
    
    








