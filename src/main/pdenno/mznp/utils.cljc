(ns pdenno.mznp.utils
  "Utilities for MiniZinc parsing.")

;;; These are all used in macros.clj
(def debugging? (atom false))
(def debugging-rewrite? (atom false))
(def tags (atom []))
(def locals (atom [{}]))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n))
  ;;(subs <a long string> 0 n))         about as good.
  ;;(cl-format nil "~v{~A~:*~}" n " ")) slower  for 20 spaces
  ;;(apply str (repeat n " ")))         slowest for 20 spaces 
  )



