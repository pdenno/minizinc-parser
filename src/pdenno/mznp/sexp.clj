(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]))

;;; The functions that end in a * (rewrite* and reduce-bin-ops*) are 'toplevel' and good for testing. 

(def debugging? (atom false))
(def diag (atom {}))
(def tags (atom []))
(def locals (atom [{}]))

(defmacro mzn-array-access [& body]
  ~@body) ; POD NYI

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
(defrewrite :MznModel [m]
  (reduce (fn [res item]
            (let [type (:type item)]
              (cond (= :MznConstraint type)
                    (update res :constraints conj (-> item :expr rewrite))
                    (#{:MznAssignment} type)
                    (let [[k v] (rewrite item)]
                      (assoc-in res [:assignments k] v)))))
          {:constraints []
           :assignments {}}
          (:items m)))

;;; This needed only for testing constraints; see above for typical usage.
(defrewrite :MznConstraint [m]
  (-> m :expr rewrite))

(defrewrite :MznExpr [m]
  (as-> m ?m
    (cond (not (:tail ?m)) (-> m :atom rewrite)
          (= :MznExprBinopTail (-> ?m :tail :type))
          (-> ?m reduce-bin-ops :bin-ops order-bin-ops)
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
    `(~'mzn-array-access ~(-> m :head rewrite)
                       ~@(-> m :tail rewrite))
    (-> m :head rewrite)))

(defrewrite :MznArray [m]
  (mapv rewrite (:elems m)))

(defrewrite :MznArrayAccess [m]
  (mapv rewrite (:exprs m)))

(defrewrite :MznId [m]
  (-> m :name symbol))

(defrewrite :char [c]
  (let [sc (safe2symbol c)]
    (if (contains? mzn2dsl-map sc)
      (mzn2dsl-map sc)
      (throw (ex-info "Unknown syntactic character" {:char c})))))

(defrewrite :keyword [k]
  (if (contains? mzn2dsl-map k)
    (mzn2dsl-map k)
    (throw (ex-info "Unknown keyword" {:key k}))))

(defrewrite :default [m]
  m)

;;;(forall [[j Jobs]]
;;;  (<= (mznIdx endWeek j) (mznIdx WeeksTillDue j)))
(defrewrite :MznGenCallExpr [m]
  (let [op (-> m :gen-call-op util/keysym)
        gens (mapv rewrite (-> m :generators))
        where (-> m :where rewrite)
        body (-> m :body rewrite)]
    `(~op ~gens :where ~(or where true) ~body)))

(defrewrite :MznGenerator [m]
  (conj (mapv rewrite (:ids m))
        (rewrite (:expr m))))

(defrewrite :MznAssignment [m]
  [(-> m :lhs rewrite keyword) (-> m :rhs rewrite)])

#_(defrewrite :bin-op [exp] ; <====================== caller should make :bin-op-seq This would handle it.
  `(~(:op exp) ~(-> exp :lhs rewrite) ~(-> exp :rhs rewrite)))
 
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
   :..-op     {:assoc :none :val 500}
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
  "Some characters don't convert!"
  [c]
  (let [these-fail {\< '<, \> '>, \= 'assign, \+ '+, \- '-, \* '* \/ '/}]
    (or (get these-fail c)
        (symbol c))))

;;; The DSL operators are just the Mzn keywords things as symbols. (This will probably change; ..-op yuk!)
(def mzn2dsl-map 
  (zipmap (keys op-precedence-table)
          (map util/keysym (keys op-precedence-table))))

(defn precedence [op]
  (if (contains? op-precedence-table op)
    (-> op op-precedence-table :val)
    100))

(defn rewrite*
  "mzn/parse-string and sexp-simplify, but with controls for partial evaluation, debugging etc.
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

;;;============================== Reduce-bin-ops ===================================
(defn primary? [exp] (-> exp :atom :head :primary?))

(defn simplify-primary 
  "Primary (which is syntactically '(' <expr> ')') isn't identical with expr; this makes it so."
  [exp]
  (update-in exp [:atom :head] #(dissoc % :primary?)))

(defn reduce-bin-ops
  "Replace nested binary operations/operands with a flat vector (bvec) [operand, op, operand...] in :bin-ops"
  [exp]
  (cond (primary? exp) ; Thing has parentheses around it. 
        (-> exp simplify-primary reduce-bin-ops),
        (and (= (:type exp) :MznExpr) (:tail exp))
        (let [tail    (-> exp :tail :expr reduce-bin-ops)
              bin-ops (-> (or (:bin-ops exp) []) ; Reduce the head and op... 
                          (conj (-> exp :atom :head reduce-bin-ops))
                          (conj (-> exp :tail :bin-op))
                          (into (-> tail :bin-ops)))]
          (as-> exp ?e
            (assoc  ?e :bin-ops bin-ops)
            (dissoc ?e :atom) ; eliminate the parts reduced (atom and tail). 
            (dissoc ?e :tail))),
        (= (:type exp) :MznExpr) ; no tail. 
        (-> exp
            (cond-> (not (:bin-ops exp)) (assoc :bin-ops []))
            (update :bin-ops #(conj % (-> exp :atom :head reduce-bin-ops)))
            (dissoc :atom)),
        (map? exp) ; this is the 'preserve' condition
        (reduce-kv 
         (fn [e k v] (assoc e k (reduce-bin-ops v)))
         {}
         exp),
        :else
        exp)) ; this is the 'recursion cutoff' condition

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
  "Process a :bin-ops vector into sexps conforming to precedence rules."
  [bvec]
  (let [bvec (mapv #(if (:bin-ops %) (-> % :bin-ops order-bin-ops) %) bvec)] ; Recursively process primaries to args. 
    (as-> (bvec2info bvec) ?info
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
                                      :form (list (:op ?omap)
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
(defn reduce-bin-ops* [str & {:keys [rewrite? reduce? file?] :as opts}]
  "Convenience function for testing reduce-bin-ops and order-bin-ops."
  (let [all? (not (or (contains? opts :rewrite?)
                      (contains? opts :reduce?)))
        mznp-struct (rewrite* ::mznp/expr (if file? (slurp str) str) :simplify? true)]
    (cond-> mznp-struct
      (or all? reduce?) reduce-bin-ops
      all? :bin-ops
      all? order-bin-ops)))


