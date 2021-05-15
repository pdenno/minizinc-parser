(ns pdenno.mznp.macros
  "ClojureScript-compatible macros for mznp."
  (:require
   [pdenno.mznp.utils :refer [debugging? debugging-rewrite? tags locals nspaces]]))

;;; (1) Macros don't have to be written in .clj; .cljc is fine. See https://clojurescript.org/about/differences:
;;;     "Macros are written in *.clj or *.cljc files and are compiled either as Clojure when using regular ClojureScript
;;;      or as ClojureScript when using bootstrapped / self-host ClojureScript."
;;; (2) In general, when writing macros for ClojureScript-compatible code, don't use ns aliases.

;;;================================= mznp.cljc =================================================
(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod ~'pdenno.mznp.mznp/parse ~tag [~'tag ~pstate ~@(or keys-form '(& _))] ; POD Why ~'tag? 
     (when @debugging? (println (str "\n" (nspaces (-> ~pstate :tags count)) "==> " ~tag)))
     (as-> ~pstate ~pstate
       (update-in ~pstate [:tags] conj ~tag)
       (update-in ~pstate [:local] #(into [{}] %))
       (if (:error ~pstate) ; Stop things
	 ~pstate
	 (try ~@body
              ~(if (:ns &env) ; See https://clojure.org/reference/macros for &env in macros. Also helins/medium. 
                 `(catch js/Error  e# {:error (str e#)         :pstate ~pstate})
                 `(catch Exception e# {:error (.getMessage e#) :pstate ~pstate}))))
       (cond-> ~pstate (not-empty (:tags ~pstate)) (update-in [:tags] pop))
       (update-in ~pstate [:local] #(vec (rest %)))
       (do (when @debugging?
             (println (str "\n"(nspaces (-> ~pstate :tags count)) "--> " ~tag (:result ~pstate))))
	   ~pstate))))

;;; This is an abstraction over protecting :result while something else swapped in...
(defmacro store [ps key & [from]]
  `(let [ps# ~ps
         key# ~key]
     (assoc-in ps# [:local 0 key#]
               (~(or from :result) ps#))))

;;; ...and this is for getting the value back. 
(defmacro recall [ps tag]
  `(let [ps# ~ps]
     (-> ~ps :local first ~tag)))

;;;================================= sexp.cljc =================================================
;;; Similar to mznp/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite. 
(defmacro defrewrite [tag [obj & keys-form] & body]
  `(defmethod ~'pdenno.mznp.sexp/rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when @debugging-rewrite? (println (str (nspaces (count @tags)) ~tag "==> ")))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (try ~@body
                        ~(if (:ns &env)
                            `(catch js/Error  e# {:error (str e#)         :rewrite-error? true})
                            `(catch Exception e# {:error (.getMessage e#) :rewrite-error? true})))]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging-rewrite? (println (str  (nspaces (count @tags)) "<-- " ~tag result#)))
         result#))))

;;;================================= sexp_test.cljc =================================================
(defmacro debug-off [& body]
   `(let [rewr-db?# @debugging-rewrite?
          mznp-db?# @debugging?]
      (reset! debugging?         false)
      (reset! debugging-rewrite? false)
      ~@body
      (reset! debugging-rewrite? rewr-db?#)
      (reset! debugging?         mznp-db?#)))

;;;=============================== mzn_fns.cljc ====================================================
(defn for-args
  "Args can look like e.g. [[lin Lines] [w1 w2 Weeks]]. They need to match syntax of clojure/for."
  [args]
  (let [split (reduce (fn [res arg-set]
                        (let [vars (butlast arg-set)
                              src  (last arg-set)]
                          (into res (mapv #(vector % src) vars))))
                      []
                      args)]
    (reduce (fn [res v]
              (-> res
                  (conj (first v))
                  (conj (second v))))
            [] split)))

(defmacro forall [args where body]
  `(let [current# (atom true)]
     (doseq ~(for-args args)
        (when (deref current#) ; POD doesn't exit early. 
          (when ~where (swap! current# (fn [arg#] ~body))))) ; arg not used.
     (deref current#)))

(defmacro exists [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when (-> current# deref not) ; POD doesn't exit early. 
         (when ~where (swap! current# (fn [arg#] ~body))))) ; arg not used.
     (deref current#)))

(defmacro sum [args where body]
  `(let [current# (atom 0)]
     (doseq ~(for-args args)
        (when ~where (swap! current# (fn [arg#] (+ arg# ~body)))))
     (deref current#)))

(defmacro mzn-max [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when ~where
         (swap! current# #(let [bigger?# ~body]
                            (if (or (-> current# deref not)
                                    (> bigger?# %))
                              bigger?#
                              %)))))
     (deref current#)))

(defmacro mzn-min [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when ~where
         (swap! current# #(let [smaller?# ~body]
                            (if (or (-> current# deref not)
                                    (< smaller?# %))
                              smaller?#
                              %)))))
     (deref current#)))
