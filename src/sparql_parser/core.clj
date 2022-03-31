(ns sparql-parser.core
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [instaparse.core :as insta])
  (:import [java.net URI]))

(def ^:const grammar-path "sparql.grammar")

(defn load-grammar
  [f]
  (let [p (io/resource f)
        grammar (slurp p)]
    (insta/parser grammar :output-format :hiccup :auto-whitespace :standard)))

(def sparql-parse (load-grammar grammar-path))

(defn uri
  [u]
  (URI. u))

(defn sparql-tx
  [form]
  (cond
    (vector? form)
    (let [[t & [v :as args]] form]
      (case t
        :PN_CHARS_BASE v
        :PN_CHARS_U v
        :VARNAME (apply str args)
        :IRIREF (URI. (apply str (rest (butlast args))))

        (:VAR1 :VAR2) (symbol (apply str args))
        (:Var :VarOrTerm :GraphTerm) v
        :QueryUnit v
        :Query (let [[p qt values] args]
                 (cond-> {:prolog p
                          :query qt}
                   values (assoc :values values)))
        :Prologue v
        :SelectQuery (apply merge args)
        :SelectClause {:find (vec (rest args))}
        :WhereClause (let [elts (if (= v "WHERE") (rest args) args)]
                       (println "elts: " elts)
                       {:where (vec (if (and (= 1 (count elts)) (= 'and (ffirst elts)))
                                      (rest (first elts))
                                      elts))})
        :GroupGraphPattern (second args)
        :GroupGraphPatternSub (list* 'and v) ;; is this right?
        :TriplesBlock (->> args (remove #(= "." %)) (map vec)) 
        :PropertyListPathNotEmpty args
        :TriplesSameSubjectPath (into [v] (second args))
        (:VerbPath :Path :PathEltOrInverse :ObjectPath :GraphNodePath) v
        :PathAlternative (if (= 1 (count args)) v [:not-implemented args])
        :PathSequence (if (= 1 (count args)) v [:not-implemented args])
        :PathElt (if-let [m (second args)] [v m] v)
        :PathPrimary (cond
                       (instance? URI v) v
                       (= "a" v) :rdf/type
                       (= "!" v) (list 'not (set args))
                       :default v)
        :ObjectListPath (if (= 1 (count args)) v [:not-implemented args]) 
        :ValuesClause v
        :SolutionModifier (and v {:modifier v})
        :iri v
        form))

    :default form))

(defn parse
  [q]
  (let [cst (sparql-parse q)]
    (walk/postwalk sparql-tx cst)))

