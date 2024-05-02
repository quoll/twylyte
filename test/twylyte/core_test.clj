(ns twylyte.core-test
  (:require [twylyte.core :refer [load-grammar sparql-tx parse]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-load-grammar
  (is (map? (load-grammar "sparql.grammar"))))
