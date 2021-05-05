(ns pdenno.mznp.macros
  (:require
   [clojure.pprint :refer (cl-format)]
   [pdenno.mznp.utils :refer [debugging? debugging-rewrite? tags locals] :as util]))

;;;================================= mznp.cljc =================================================
(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod ~'pdenno.mznp.mznp/parse ~tag [~'tag ~pstate ~@(or keys-form '(& ignore))]
     (when @debugging? (printf "%n%s==> %s" (util/nspaces (-> ~pstate :tags count)) ~tag))
     (as-> ~pstate ~pstate
       (update-in ~pstate [:tags] conj ~tag)
       (update-in ~pstate [:local] #(into [{}] %))
       (if (:error ~pstate) ; Stop things
         ~pstate
         (try ~@body
              (catch Exception e# {:error (.getMessage e#)
                                  :pstate ~pstate})))
       (cond-> ~pstate (not-empty (:tags ~pstate)) (update-in [:tags] pop))
       (update-in ~pstate [:local] #(vec (rest %)))
       (do (when @debugging? (cl-format *out* "~%~A<-- ~A" (util/nspaces (-> ~pstate :tags count)) ~tag))
           ~pstate))))

;;; Abbreviated for simple forms such as builtins. 
(defmacro defparse-auto [tag test]
  `(defparse ~tag
     [pstate#]
     (-> pstate#
         (assoc :result (:tkn pstate#))
         (pdenno.mznp.mznp/eat-token ~test))))

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
     (when @debugging-rewrite? (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (try ~@body
                        (catch Exception e# {:error (.getMessage e#)
                                             :rewrite-error? true}))]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging-rewrite? (cl-format *out* "~A<-- ~A returns ~S~%" (util/nspaces (count @tags)) ~tag result#))
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
