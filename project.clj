(defproject pdenno/mznp "0.1.64"
  :description "A library for MiniZinc parsing and analysis"
  :url "http://github/pdenno/minizinc-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure               "1.10.3"]
                 [org.clojure/spec-alpha2 "0.2.177-SNAPSHOT"] 
                 [com.taoensso/timbre     "5.1.2"]]
  :source-paths ["src/main"]
  :repl-options {:init-ns pdenno.mznp.mznp})
