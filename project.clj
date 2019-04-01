(defproject pdenno/minizinc-parser "0.1.0-SNAPSHOT"
  :description "A library for MiniZinc parsing and analysis"
  :url "http://github/pdenno/minizinc-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure     "1.10.0"]
                 [org.clojure/spec-alpha2 "0.2.177-SNAPSHOT"]]
  :repl-options {:init-ns pdenno.mznp.mznp})
