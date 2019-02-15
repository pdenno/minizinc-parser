(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.mznp :as mznp]))

(def op-precedence
  {
   <-> {:assoc :left :val 1200}
   ->  {:assoc :left :val 1100}
   <-  {:assoc :left :val 1100}
   \/  {:assoc :left :val 1000}
   xor {:assoc :left :val 1000}
   /\  {:assoc :left :val 900}
   \<  {:assoc :none :val 800}
   \>  {:assoc :none :val 800}
   <=  {:assoc :none :val 800}
   >=  {:assoc :none :val 800}
   ==, {:assoc :none :val 800}
   =, {:assoc :none :val 800}
   !=, {:assoc :none :val 800}
   :in {:assoc :none :val 700}
   :subset   {:assoc :none :val 700}
   :superset {:assoc :none :val 700}
   :union    {:assoc :left :val 600}
   :diff     {:assoc :left :val 600}
   :symdiff  {:assoc :left :val 600}
   ..        {:assoc :none :val 500}
+
left
400
-
left
400
*
left
300
div
left
300
mod
left
300
/
left
300
intersect left
300
++
right 200
‘?ident?‘
left
100


(def foo (mznp/->MznId "foo"))
(def bar (:result (mznp/parse-string ::mznp/expr "1 + 2")))

(declare map-simplify remove-nils rewrite)

(defn sexp-simplify
  "Toplevel function to transform the structure produced by the parser to something useful."
  [m]
  (-> m
      map-simplify
      rewrite))

(defn map-simplify
  "Recursively traverse the map structures changing records to maps, 
   removing nil map values and adding :type value named after the record."
  [m]
  (if (record? m)
    (-> {:type (-> m .getClass .getSimpleName keyword)} ; BTW, nothing from the parse has a :type. 
        (into (zipmap (keys m) (map record2map (vals m))))
        remove-nils)
    m))
        
(defn remove-nils
  "Remove map values that are nil."
  [m]
  (reduce-kv (fn [m k v]
               (if (= nil v)
                 m
                 (assoc m k v)))
             {}
             m))

(defn rewrite-dispatch [obj]
  (let [result
        (cond (map? obj)     (:type obj)
              (char? obj)    :char
              :else          nil)]
    (println "==> Dispatch to " result)
    result))

(defmulti rewrite #'rewrite-dispatch)

{:type :MznExpr,
 :atom {:type :MznExprAtom,
        :head 1},
 :tail {:type :MznExprBinopTail,
        :bin-op \+,
        :expr {:type :MznExpr, :atom {:type :MznExprAtom, :head 2}}}}

(defmethod rewrite :MznExpr [m]
  (let [atom (-> m :atom rewrite)]
    (if (contains? m :tail)
      (let [{:keys [op expr]} (-> m    
                                  :tail
                                  rewrite)]
        `(~op ~atom ~expr))
      atom)))

(defmethod rewrite :MznExprBinopTail [m]
  {:op   (-> m :bin-op rewrite)
   :expr (-> m :expr   rewrite)})

(defmethod rewrite :MznExprAtom [m]
  (-> m :head rewrite))

(defmethod rewrite :MznId [m]
  (-> m :name symbol))

(defmethod rewrite :char [m]
  (-> m str symbol))

(defmethod rewrite nil [m]
  (println "pass through " m)
  m)



;;; Diagnostic
(defn test-rewrite [tag str]
  (let [db? @mznp/debugging?]
    (reset! mznp/debugging? false)
    (let [result (-> (mznp/parse-string tag str)
                     :result
                     sexp-simplify)]
      (reset! mznp/debugging? db?)
      result)))
