(ns pdenno.mznp.mzn-user
  "Not much here. It is where the user's data structures and code for constraints is interned."
  (:refer-clojure :exclude [range max min])
  (:require [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.gen :as gen]
            [clojure.spec-alpha2.test :as test]
            [pdenno.mznp.mzn-fns :refer :all]))

(alias 'mznf 'pdenno.mznp.mzn-fns)
      


