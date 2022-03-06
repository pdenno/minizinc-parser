(ns pdenno.mznp.utils
  "Utilities for MiniZinc parsing and rewriting.")

;;;============ These are all used in macros.clj The macros are used in mznp.cljc and rewrite.cljc ==============
(def debugging? (atom false))
(def debugging-rewrite? (atom false))
(def tags (atom []))
(def locals (atom [{}]))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

(defn class-name
  "Return a keyword representing the class of the object.
  For example (class-name 1) ==> :Long. Class name is not namespace-qualified."
  [obj]
  #?(:clj  (->> obj type str                      (re-matches #"^.+\.(.*)$") second keyword)
     :cljs (->> (with-out-str (print (type obj))) (re-matches #"^.+/(.*)$")  second keyword)))

(defn index-of-elem
  "ClojureScript-compatible version of .indexOf."
  [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) -1
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))
