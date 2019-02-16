(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]))

(def debugging? (atom true))
(def tags (atom []))
(def locals (atom [{}]))

;;; Similar to mznp/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite. 
(defmacro defrewrite [tag [obj & keys-form] & body] 
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when @debugging? (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# ~@body]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging? (cl-format *out* "~A<-- ~A returns ~S~%" (util/nspaces (count @tags)) ~tag result#))
         result#))))

(declare map-simplify remove-nils rewrite op-precedence)

(defn sexp-simplify
  "Toplevel function to transform the structure produced by the parser to something useful."
  [m]
  (-> m
      map-simplify
      rewrite))

(defn map-simplify
  "Recursively traverse the map structures changing records to maps, 
   removing nil map values, and adding :type value named after the record."
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

(defn rewrite-dispatch [tag obj & keys] tag)

(defmulti rewrite-meth #'rewrite-dispatch)

(defn rewrite [obj & keys]
  (let [tag
        (cond (map? obj)     (:type obj)
              (char? obj)    :char
              :else          :default)]
    (rewrite-meth tag obj keys)))

(defrewrite :MznExpr [m]
  (let [atom (-> m :atom rewrite)]
    (cond (not (:tail m)) atom,
          (= :MznExprBinopTail (-> m :tail :type)) (assoc (-> m :tail rewrite) :lhs atom), 
          :else (throw (ex-info "Some other tail" {:tail (:tail m)})))))

(defrewrite :MznExprBinopTail [m]
  {:op   (-> m :bin-op rewrite)
   :rhs  (-> m :expr   rewrite)})

(defrewrite :MznExprAtom [m]
  (-> m :head rewrite))

(defrewrite :MznId [m]
  (-> m :name symbol))

(defrewrite :char [m]
  (-> m str symbol))

(defrewrite :default [m]
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

(defn test-map-simplify [tag str]
  (let [db? @mznp/debugging?]
    (reset! mznp/debugging? false)
    (let [result (-> (mznp/parse-string tag str)
                     :result
                     map-simplify)]
      (reset! mznp/debugging? db?)
      result)))

;;; MiniZinc Specification, section 7.2. 
(def op-precedence ; lower :val means binds tighter. 
  {:<->-op {:assoc :left :val 1200}
   :->-op  {:assoc :left :val 1100}
   :<--op  {:assoc :left :val 1100}
   :or-op  {:assoc :left :val 1000}
   :xor    {:assoc :left :val 1000}
   :and-op {:assoc :left :val 900}
   \<      {:assoc :none :val 800}
   \>      {:assoc :none :val 800}
   :le-op  {:assoc :none :val 800}
   :ge-op  {:assoc :none :val 800}
   :eq-op  {:assoc :none :val 800}
   \=      {:assoc :none :val 800}
   :ne-op  {:assoc :none :val 800}
   :in     {:assoc :none :val 700}
   :subset    {:assoc :none :val 700}
   :superset  {:assoc :none :val 700}
   :union     {:assoc :left :val 600}
   :diff      {:assoc :left :val 600}
   :symdiff   {:assoc :left :val 600}
   :..-op     {:assoc :none :val 500}
   \+         {:assoc :left :val 400}
   \-         {:assoc :left :val 400}
   \*         {:assoc :left :val 300}
   :div       {:assoc :left :val 300}
   :mod       {:assoc :left :val 300}
   \/         {:assoc :left :val 300}
   :intersec  {:assoc :left :val 300}   
   :++-op     {:assoc :right :val 200}
   :<ident>   {:assoc :left  :val 100}})
