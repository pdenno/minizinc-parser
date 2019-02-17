(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]))

(def debugging? (atom false))
(def tags (atom []))
(def locals (atom [{}]))

;;; Similar to mznp/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite. 
(defmacro defrewrite [tag [obj & keys-form] & body] 
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when @debugging? (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (do ~@body)]
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
      rewrite
      fix-precedence))

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
  (let [atom (-> m :atom rewrite)
        primary? (:primary? m)] ; Need to grab it now. 
    (as-> m ?m
      (cond (not (:tail ?m)) atom,
            (= :MznExprBinopTail (-> ?m :tail :type))
            (into {:type :bin-op :lhs atom } (-> ?m :tail rewrite)), ; into: get :lhs? on the lhs
            :else (throw (ex-info "Some other tail" {:tail (:tail ?m)})))
      (cond-> ?m
        primary? (assoc :type :bin-op-primary)))))

(defrewrite :MznExprBinopTail [m]
  {:op    (-> m :bin-op rewrite)
   :rhs   (-> m :expr   rewrite)})

(defrewrite :MznExprAtom [m]
  (-> m :head rewrite))

(defrewrite :MznId [m]
  (-> m :name symbol))

(defrewrite :char [m]
  (-> m str symbol))

(defrewrite :default [m]
  m)

;;; (test-rewrite ::mznp/expr "1*2-3") => {:lhs 1, :op *, :rhs? {:lhs 2, :op -, :rhs? 3}} *Not as intended!*
;;; (def foo   {:lhs 1,                      :op '*, :rhs {:lhs 2, :op '-, :rhs 3}})
;;; (def defoo {:lhs {:lhs 1 :op '* :rhs 2}, :op '-, :rhs 3})

;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * binds tighter than +. 
(defn fix-precedence [exp]
  (if (map? exp)
    (let [op1 (:op exp)
          op2 (-> exp :rhs :op)]
      (if (< (precedence op1) (precedence op2))
        (let [rhs-save (:rhs exp)]
          (-> exp
              (assoc :lhs {:lhs (:lhs exp) :op (:op exp) :rhs (-> exp :rhs :lhs)})
              (assoc :op (:op rhs-save))
              (assoc :rhs (:rhs rhs-save))
              (update :lhs fix-precedence)
              (update :rhs fix-precedence)))
        exp))
    exp))

(defn make-sexp [m]
  (if (map? m)
    `(~(:op m) ~(make-sexp (:lhs m)) ~(make-sexp (:rhs m)))
    m))

;;; MiniZinc Specification, section 7.2.
;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * binds tighter than +.

(def op-precedence-table ; lower :val means binds tighter. 
  {:<->-op {:assoc :left :val 1200}
   :->-op  {:assoc :left :val 1100}
   :<--op  {:assoc :left :val 1100}
   :or-op  {:assoc :left :val 1000}
   :xor    {:assoc :left :val 1000}
   :and-op {:assoc :left :val 900}
   '<      {:assoc :none :val 800}
   '>      {:assoc :none :val 800}
   :le-op  {:assoc :none :val 800}
   :ge-op  {:assoc :none :val 800}
   :eq-op  {:assoc :none :val 800}
   '=      {:assoc :none :val 800}
   :ne-op  {:assoc :none :val 800}
   :in     {:assoc :none :val 700}
   :subset    {:assoc :none :val 700}
   :superset  {:assoc :none :val 700}
   :union     {:assoc :left :val 600}
   :diff      {:assoc :left :val 600}
   :symdiff   {:assoc :left :val 600}
   :..-op     {:assoc :none :val 500}
   '+         {:assoc :left :val 400}
   '-         {:assoc :left :val 400}
   '*         {:assoc :left :val 300}
   :div       {:assoc :left :val 300}
   :mod       {:assoc :left :val 300}
   '/         {:assoc :left :val 300}
   :intersec  {:assoc :left :val 300}   
   :++-op     {:assoc :right :val 200}
   :<ident>   {:assoc :left  :val 100}})

(defn precedence [op]
  (if (contains? op-precedence-table op)
    (-> op op-precedence-table :val)
    100))

(def ops-with-precedence
  (reduce-kv (fn [m k v] (assoc m k (map first v)))
             {}
             (group-by second (map #(let [[k v] %] [k (:val v)])
                                   op-precedence-table))))

(defn test-rewrite
  "mzn/parse-string and sexp-simplify, but with controls for partial evaluation, debugging etc.
   With no keys it does all steps without debug output."
  [tag str & {:keys [simplify? rewrite? fix? debug? mznp-debug?] :as opts}]
  (let [all? (not (or (contains? opts :simplify?)
                      (contains? opts :rewrite?)
                      (contains? opts :fix?)))
        mznp-db? @mznp/debugging?
        db?           @debugging?]
    (reset! mznp/debugging? mznp-debug?)
    (reset!      debugging? debug?)
    (let [result (-> (mznp/parse-string tag str)
                     :result
                     (cond->
                         (or all? fix? rewrite? simplify?) map-simplify
                         (or all? fix? rewrite?)           rewrite
                         (or all? fix?)                    fix-precedence
                         all?                              make-sexp))]
      (reset! mznp/debugging? mznp-db?)
      (reset!      debugging?      db?)
      result)))


(defn flatten-binop
  ([exp] (flatten-binop exp []))
  ([exp res]
   (cond (= (:type exp) :bin-op)
         (-> res 
             (into [(flatten-binop (:lhs exp))])
             (conj (:op exp))
             (into [(flatten-binop (:rhs exp))])),
         (= (:type exp) :bin-op-primary)
         (conj res
               (vector (flatten-binop (:lhs exp))
                       (:op exp)
                       (flatten-binop (:rhs exp)))),
         :else exp)))
         
         
        
                 
               
