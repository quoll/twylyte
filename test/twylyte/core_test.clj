(ns twylyte.core-test
  (:require [twylyte.core :refer [load-grammar sparql-tx parse]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-load-grammar
  (is (map? (load-grammar "sparql.grammar"))))

(def var-sparql "SELECT ?s WHERE { ?s ?p ?o }")

(def lvar-sparql "SELECT ?subject WHERE { ?subject ?predicate ?object }")

(def curie-sparql "select ?s where { ?s rdf:type ?o }")

(def prefix-sparql "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nselect ?s where { ?s rdf:type ?o }")

(def iri-sparql "SELECT ?title\nWHERE\n{\n  <http://example.org/book/book1> <http://purl.org/dc/elements/1.1/title> ?title .\n}")
