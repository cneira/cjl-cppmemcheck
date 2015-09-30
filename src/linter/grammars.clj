(ns linter.grammars
  (:require [instaparse.core :as insta]
            [clojure.string :as str ]
            [linter.ebnf-cpp :as gram]
            ))

(def memory-allocation-heap
  (insta/parser mem-alloc-grammar  ))


(def memory-deallocation-heap
  (insta/parser mem-dealloc-grammar  ))



(memory-allocation-heap "char p = new char variable[23];\n int p[2];  ")

