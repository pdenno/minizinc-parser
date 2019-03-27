(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]))

;;; The functions that end in a * (rewrite* and form-bin-ops*) are 'toplevel' and good for testing. 

(def debugging? (atom false))
(def diag (atom {}))
(def tags (atom []))
(def locals (atom [{}]))

(defmacro aacc-op [& body]
  `(aacc-op ~@body)) ; NYI

(defmacro range-op [& body]
  `(range-op ~@body))

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

(declare map-simplify remove-nils rewrite precedence op-precedence mzn2dsl-map safe2symbol)
(declare order-bin-ops reduce-bin-ops)

(defn map-simplify
  "Recursively traverse the map structures changing records to maps, 
   removing nil map values, and adding :type value named after the record."
  [m]
  (cond (record? m)
        (-> {:type (-> m .getClass .getSimpleName keyword)} ; BTW, nothing from the parse has a :type. 
            (into (zipmap (keys m) (map map-simplify (vals m))))
            remove-nils),
        (vector? m) (mapv map-simplify m),
        :else m))
        
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
              (keyword? obj) :keyword
              :else          :default)]
    (rewrite-meth tag obj keys)))

;;;----------------------- Rewriting ---------------------------------------
;;;----------------------- Top-level ---------------------------------------
(defrewrite :MznModel [m]
  (reduce (fn [res item]
            (let [type (:type item)]
              (cond (= type :MznConstraint)
                    (update res :constraints conj (rewrite item))
                    (= type :MznVarDecl)
                    (let [vd (rewrite item)]
                      (assoc-in res [:var-decls (:name vd)] vd))
                    (= type :MznSolve)
                    (assoc res :solve (rewrite item))
                    :else res)))
          {:constraints [] :var-decls {}}
          (:items m)))

(defrewrite :MznConstraint [m]
  (-> m :expr rewrite))

(defrewrite :MznVarDecl [m]
  (let [res {:name (-> m :lhs :id rewrite str)
             :vartype (-> m :lhs rewrite)
             :init (-> m :rhs rewrite)}]
    (cond-> res
      (:var? m) (assoc :var? true))))

(defrewrite :MznSolve [m]
  {:action (:action m)
   :expr (-> m :expr rewrite)})

;;;-------------------- Types ---------------------------------
(defrewrite :MznTypeInstExpr [m]
  (-> m :expr rewrite))

(defrewrite :MznSetType [m]
  {:datatype :mzn-set
   :base-type (-> m :base-type rewrite)})

(defrewrite :MznArrayType [m]
  (let [idx (mapv rewrite (:index m))]
    {:datatype (if (== 2 (count idx)) :mzn-2d-array :mzn-array)
     :index idx
     :base-type (-> m :base-type rewrite)}))

(defrewrite :MznIdDef [m]
  (if (-> m :id-type :expr keyword?)
    {:datatype (-> m :id-type rewrite)}
    (-> m :id-type rewrite)))

;;;-------------------- Literals ---------------------------------
(defrewrite :MznArray [m]
  (mapv rewrite (:elems m)))

(defrewrite :Mzn2dArray [m]
  (mapv #(mapv rewrite %) (:sublists m)))


;;;-------------------- Expressions ---------------------------
(defrewrite :MznExpr [m]
  (as-> m ?m
    (cond (not (:tail ?m)) (-> m :atom rewrite)
          (= :MznExprBinopTail (-> ?m :tail :type))
          (map rewrite (-> ?m reduce-bin-ops :bin-ops order-bin-ops)) ; reduce-bin-op handles primaries (nested exprs)
          :else (throw (ex-info "Some other tail" {:tail (:tail ?m)})))))

(defrewrite :MznIfExpr [m]
  `(if ~(-> m :condition rewrite)
     ~(-> m :then rewrite)
     ~(-> m :else rewrite)))

(defrewrite :MznExprBinopTail [m]
  {:op    (-> m :bin-op rewrite)
   :rhs   (-> m :expr   rewrite)})

(defrewrite :MznExprAtom [m]
  (if (contains? m :tail)
    `(~'aacc-op ~(-> m :head rewrite)
                       ~@(-> m :tail rewrite))
    (-> m :head rewrite)))

(defrewrite :MznArrayAccess [m]
  (mapv rewrite (:exprs m)))

(defrewrite :MznId [m]
  (-> m :name symbol))

(defrewrite :char [c]
  (let [sc (safe2symbol c)]
    (if (contains? mzn2dsl-map sc)
      (mzn2dsl-map sc)
      (throw (ex-info "Unknown syntactic character" {:char c})))))

#_(def mzn-keyword? (set (map keyword mznp/mzn-keywords)))

(defrewrite :keyword [k]
  (cond (contains? mzn2dsl-map k) (mzn2dsl-map k)     ; these are operators
        #_(mzn-keyword k) #_k
        (#{:int :float} k) k                          ; there are mzn keywords (POD do I really only want :int and :float
        :else (throw (ex-info "Unknown keyword" {:key k}))))

(defrewrite :default [m]
  m)

;;;(forall [[j Jobs]]
;;;  (<= (mznIdx endWeek j) (mznIdx WeeksTillDue j)))
(defrewrite :MznGenCallExpr [m]
  (let [op (-> m :gen-call-op util/keysym)
        gens (mapv rewrite (-> m :generators))
        where (-> m :where rewrite)
        body (-> m :body rewrite)]
    `(~op ~gens  ~(or where true) ~body)))

(defrewrite :MznGenerator [m]
  (conj (mapv rewrite (:ids m))
        (rewrite (:expr m))))

(defrewrite :MznAssignment [m]
  [(-> m :lhs rewrite keyword) (-> m :rhs rewrite)])

(defrewrite :id-type [m]
  (rewrite m))

;;;============================== Precedence ordering ===================================================
;;;============================== form-bin-ops* = (-> reduce-bin-ops order-bin-ops) =====================

;;; MiniZinc Specification, section 7.2.
;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
(def op-precedence-tbl ; lower :val means binds tighter. 
  {:<->-op    {:assoc :left :val 1200}
   :->-op     {:assoc :left :val 1100}
   :<--op     {:assoc :left :val 1100}
   :or-op     {:assoc :left :val 1000}
   :xor       {:assoc :left :val 1000}
   :and-op    {:assoc :left :val 900} 
   '<         {:assoc :none :val 800}
   '>         {:assoc :none :val 800}
   :<=        {:assoc :none :val 800}
   :>=        {:assoc :none :val 800}
   :==        {:assoc :none :val 800}
   'assign    {:assoc :none :val 800}
   :not=      {:assoc :none :val 800}
   :in        {:assoc :none :val 700}
   :subset    {:assoc :none :val 700}
   :superset  {:assoc :none :val 700}
   :union     {:assoc :left :val 600}
   :diff      {:assoc :left :val 600}
   :symdiff   {:assoc :left :val 600}
   :range-op  {:assoc :none :val 500}
   '+         {:assoc :left :val 400}
   '-         {:assoc :left :val 400}
   '*         {:assoc :left :val 300}
   :div       {:assoc :left :val 300}
   :mod       {:assoc :left :val 300}
   '/         {:assoc :left :val 300}
   :intersect {:assoc :left :val 300}   
   :++-op     {:assoc :right :val 200}
   :<ident>   {:assoc :left  :val 100}})

(defn safe2symbol
  "Some characters don't convert to symbols!"
  [c]
  (let [these-fail {\< '<, \> '>, \= 'assign, \+ '+, \- '-, \* '* \/ '/}]
    (if (keyword? c)
      c
      (or (get these-fail c)
          (symbol c)))))

;;; The DSL operators are just the Mzn keywords things as symbols. 
(def mzn2dsl-map 
  (zipmap (keys op-precedence-tbl)
          (map util/keysym (keys op-precedence-tbl))))

(defn precedence [op]
  (if (contains? op-precedence-tbl op)
    (-> op op-precedence-tbl :val)
    100))

(defn rewrite*
  "mzn/parse-string, simplify, and rewrite, but with controls for partial evaluation, debugging etc.
   With no keys it does all steps without debug output."
  [tag str & {:keys [none? simplify? rewrite? file? debug? debug-mznp?] :as opts}]
  (let [all? (not (or (contains? opts :simplify?)
                      (contains? opts :rewrite?)
                      (contains? opts :none?)))
        mznp-db? @mznp/debugging?
        db?           @debugging?]
    (reset! mznp/debugging? debug-mznp?)
    (reset!      debugging? debug?)
    (let [result (-> (mznp/parse-string tag (if file? (slurp str) str))
                     :result
                     (cond->
                         (or all? rewrite? simplify?) map-simplify
                         (or all? rewrite?)           rewrite))] 
      (reset! mznp/debugging? mznp-db?)
      (reset!      debugging?      db?)
      result)))

;;; In the case of a + b   the Expr      has a :tail
;;; In the case of a[i]    the Expr.atom has a :tail
(defn reduce-bin-ops
  "Replace nested binary operations/operands with a flat vector (bvec) [operand, op, operand...] in :bin-ops"
  [exp]
  (cond (and (= (:type exp) :MznExpr) (:tail exp)) ; a + b
        (let [tail    (-> exp :tail :expr reduce-bin-ops :bin-ops)
              bin-ops (-> (or (:bin-ops exp) []) ; Reduce the head and op... 
                          (conj (-> exp :atom rewrite reduce-bin-ops)) 
                          (conj (-> exp :tail :bin-op))
                          (into tail))]
          (as-> exp ?e
            (assoc  ?e :bin-ops bin-ops)
            (dissoc ?e :atom) ; eliminate the parts reduced (atom and tail). 
            (dissoc ?e :tail))),
        (= (:type exp) :MznExpr) ; no tail (Array access and others). 
        (-> exp
            (cond-> (not (:bin-ops exp)) (assoc :bin-ops []))
            (update :bin-ops #(conj % (-> exp :atom rewrite reduce-bin-ops)))
            (dissoc :atom)),
        (map? exp) ; this is the 'preserve' condition
        (reduce-kv 
         (fn [e k v] (assoc e k (reduce-bin-ops v)))
         {}
         exp),
        :else
        exp)) ; this is the 'recursion cutoff' condition

(s/check-asserts true)
(def operator? (-> op-precedence-tbl keys set))
(s/def ::op operator?)
(s/def ::pos  (s/and integer? pos?)) ; I *think* pos?
(s/def ::prec (s/and integer? pos?))
(s/def ::info-op  (s/keys :req-un [::pos ::op ::prec]))
(s/def ::operators (s/coll-of ::info-op :kind vector?))
(s/def ::info (s/keys :req-un [::args ::operators]))

(defn bvec2info
  "Using the bin-op-vec (at any level of processing), create a map containing information about it."
  [bvec]
  (reduce
   (fn [res [k v]]
     (if (odd? k)
       (-> res
           (update :operators #(let [sym (safe2symbol v)]
                                 (conj % {:pos k
                                          :op sym
                                          :prec (-> op-precedence-tbl sym :val)})))
           (update :args #(conj % :$op$)))
       (update res :args #(conj % v))))
   {:operators [] :args []}
   (map #(vector %1 %2) (range (count bvec)) bvec)))

(defn update-op-pos [info]
  "Update the :pos values in operators according to new shortened :operands."
  (let [args (:args info)
        replace-pos (reduce (fn [positions [idx v]]
                              (if (= v :$op$)
                                (conj positions idx)
                                positions))
                            []
                            (map #(vector %1 %2) (-> args count range) args))]
    (assoc info
           :operators
           (loop [operators (:operators info)
                  result []
                  positions replace-pos]
             (let [operator (first operators)
                   is-op? (contains? operator :op)]
               (if (empty? operators)
                 result
                 (recur
                  (rest operators)
                  (if is-op?
                    (conj result (assoc operator :pos (first positions)))
                    (conj result nil)) ; placeholder needed by calling function.
                  (if is-op? (rest positions) positions))))))))

(defn update-args
  "Remove used operands and replace with sexps." 
  [info mod-pos]
  (update info
            :args
            (fn [args]
              (reduce (fn [o pos]
                        (cond (or (= mod-pos (dec pos)) (= mod-pos (inc pos)))  ;; eliminated
                              o,
                              (= mod-pos pos)
                              (conj o (some #(when (= (:pos %) pos) (:form %))  ;; composed sexp
                                            (:operators info))),
                              :else
                              (conj o (nth args pos))))                         ;; no change
                      []
                      (range (count args))))))

(defn order-bin-ops
  "Process a :bin-ops vector into sexps conforming to precedence rules. Crazy!"
  [bvec]
  (let [bvec (mapv #(if (:bin-ops %) (-> % :bin-ops order-bin-ops) %) bvec) ; Recursively process primaries to args.
        info (bvec2info bvec)]
    (s/assert ::info info)
    (as-> info ?info
      (reduce (fn [info pval]
                (loop [index (-> info :operators count range) 
                       info info]
                  (let [ops (:operators info)]
                    (if (empty? index)
                      info
                      (let [ix (first index) ; Picks out an operator (might not be modified; see pval). 
                            omap (nth ops ix)
                            pos  (:pos omap)
                            prec (:prec omap)
                            omap (as-> omap ?omap
                                   (if (= pval prec)
                                     {:pos pos
                                      :form (list (-> ?omap :op symbol)
                                                  (nth (:args info) (dec pos))
                                                  (nth (:args info) (inc pos)))}
                                     ?omap))]
                        (recur (rest index)
                               (-> info
                                   (assoc :operators (into (conj (subvec ops 0 ix) omap)
                                                           (subvec ops (inc ix))))
                                   (cond-> (= pval prec) (update-args pos)
                                           (= pval prec) update-op-pos))))))))
              ?info
              (-> (map :prec (:operators ?info)) distinct sort))
      (-> ?info :args first))))

;;; POD TODO: Remove excessive and, or, + 
(defn form-bin-ops* [str & {:keys [rewrite? reduce? file?] :as opts}]
  "Convenience function for testing reduce-bin-ops and order-bin-ops."
  (let [all? (not (or (contains? opts :rewrite?)
                      (contains? opts :reduce?)))
        mznp-struct (rewrite* ::mznp/expr (if file? (slurp str) str) :simplify? true)]
    (cond-> mznp-struct
      (or all? reduce?) reduce-bin-ops
      all? :bin-ops
      all? order-bin-ops)))
