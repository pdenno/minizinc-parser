(defproject pdenno/minizinc-parser "0.1.0-SNAPSHOT"
  :description "A library for MiniZinc parsing and analysis"
  :url "http://github/pdenno/minizinc-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure               "1.10.1"]
                 [org.clojure/spec-alpha2 "0.2.177-SNAPSHOT"] 
                 [org.clojure/tools.logging          "0.5.0"]]
  :repl-options {:init-ns pdenno.mznp.mznp})

;;; POD ToDo: Write deps.edn for this and use the SHA available by running
;;; git ls-remote https://github.com/clojure/spec-alpha2.git refs/heads/master
;;; (See https://github.com/clojure/spec-alpha2) to make this track spec-alpha2.
;;; (Will have to learn how to do the equivalent of lein install.) 
