(ns twylyte.core
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [quoll.rdf :as rdf]
            #?(:clj  [instaparse.core :refer [parser parses defparser]]
               :cljs [instaparse.core :refer [parser parses] :refer-macros [defparser]]))
  #?(:clj (:import [java.net URI])))

(def ^:const grammar-path "resources/sparql.grammar")

(defparser sparql-parse grammar-path :output-format :hiccup :auto-whitespace :standard :string-ci true)

#?(:clj
   (defn uri [u] (URI. u))

   :cljs
   (defn uri [u] (goog.Uri. u)))

(defn iri-type? [i] (or (keyword? i) (rdf/iri? i)))

(defn ast
  "Converts elemtns of the CST (Concrete Syntax Tree) to an AST (Abstract Syntax Tree)"
  [form]
  (cond
    (vector? form)
    (let [[t & [v :as args]] form]
      (case t
        (:Var :VarOrTerm :GraphNodePath :ObjectPath :ObjectListPath :VerbSimple
              :PN_CHARS_BASE :PN_CHARS_U :PN_CHARS :PNAME_NS :PrefixedName :iri
              :Path :VerbPath
              ) v

        (:PN_LOCAL :VARNAME :PN_PREFIX) (apply str args)

        :PNAME_LN (keyword v (second args))
        (:VAR1 :VAR2) (symbol (apply str args))
        :IRIREF (rdf/iri (apply str (rest (butlast args))))
        :PathEltOrInverse (if (= "^" v)
                            (let [next-arg (second args)
                                  arg (if (= "a" next-arg) :rdf/type next-arg)
                                  property (if (iri-type? next-arg) :property :path)]
                              {:tag :reverse
                               property arg})
                            v)
        :PathSequence (if (next args)
                        {:tag :path-sequence
                         :path args}
                        v)
        :PathAlternative (if (next args)
                           {:tag :path-alternative
                            :path args}
                           v)
        :PathOneInPropertySet (cond
                                (= "a" v) :rdf/type
                                (= "^" v) (let [next-arg (second args)]
                                            {:tag :reverse
                                             :property (if (= "a" next-arg)
                                                         :rdf/type
                                                         next-arg)})
                                :default v)
        :PathPrimary (cond
                       (= "a" v) :rdf/type
                       (= "!" v) {:tag :not
                                  :path (second args)}
                       (= "(" v) {:tag :path-seq
                                  :path (butlast args)}
                       :default v)
        :PathElt (if-let [m (second args)]
                   (assoc m :path v)
                   v)
        :PathMod (case v
                   "*" {:tag :zero-or-more}
                   "+" {:tag :one-or-more}
                   "?" {:tag :zero-or-one})
        :PropertyListPathNotEmpty (let [props (if (iri-type? v) :property :path)
                                        more-props (loop [args (drop 3 args)
                                                          prop-objs []]
                                                     (if (seq args)
                                                       (let [[p o & pol] args]
                                                         (recur (rest pol) (conj prop-objs [p o])))
                                                       prop-objs))]
                                    (cond-> {:tag :property-list
                                             props v
                                             :object (second args)}
                                      (seq more-props) (assoc :more more-props)))
        :TriplesSameSubjectPath (assoc (second args)
                                       :tag :triples-same-subject
                                       :subject v)
        :TriplesBlock (if (= "." (second args))
                        (cons v (drop 2 args))
                        (if (seq? v) v (list v)))
        :GroupGraphPatternSub (if (seq? v) v (list v))
        :GroupGraphPattern (second args)
        :WhereClause (if (= (str/lower-case v) "where")
                       (second args)
                       v)
        :SelectClause (let [m (second args)
                            modifier (cond
                                       (= (str/lower-case m) "distinct") :distinct
                                       (= (str/lower-case m) "reduced") :reduced
                                       :default nil)]
                        (cond-> {:tag :select
                                 :selection (vec (if modifier (drop 2 args) (next args)))}
                          modifier (assoc :modifier modifier)))
        :PrefixDecl {:tag :prefix
                     :prefix (second args)
                     :iri (nth args 3)}
        :BaseDecl {:tag :base
                   :iri (second args)}
        :Prologue (let [prefixes (filter #(= :prefix (:tag %)) args)
                        base (last (filter #(= :base (:tag %)) args))]
                    (cond-> {:tag :prologue}
                      (seq prefixes) (assoc :prefixes prefixes)
                      base (assoc :base base)))
        :SelectQuery (let [n-args (next args)
                           datasets (take-while #(= :datasetClause (:tag %)) n-args)
                           [where modifier] (drop (count datasets) n-args)]
                       (cond-> {:find v
                                :graphs (vec (keep :graph datasets))
                                :named-graphs (vec (keep :named-graph datasets))
                                :where where}
                         modifier (assoc :solution-modifier modifier)))



        :BLANK_NODE_LABEL form
        :LANGTAG form
        :INTEGER form
        :DECIMAL form
        :DOUBLE form
        :INTEGER_POSITIVE form
        :DECIMAL_POSITIVE form
        :DOUBLE_POSITIVE form
        :INTEGER_NEGATIVE form
        :DECIMAL_NEGATIVE form
        :DOUBLE_NEGATIVE form
        :EXPONENT form
        :STRING_LITERAL1 form
        :STRING_LITERAL2 form
        :STRING_LITERAL_LONG1 form
        :STRING_LITERAL_LONG2 form
        :ECHAR form
        :NIL rdf/NIL

        :WS form
        :ANON form
        :PLX form
        :PERCENT form
        :HEX form
        :PN_LOCAL_ESC form
        :Query form
        :QueryUnit form
        :SubSelect form
        :ConstructQuery form
        :DescribeQuery form
        :AskQuery form
        :DatasetClause form
        :DefaultGraphClause form
        :NamedGraphClause form
        :SourceSelector form
        :GroupClause form
        :GroupCondition form
        :HavingClause form
        :HavingCondition form
        :OrderClause form
        :OrderCondition form
        :LimitOffsetClauses form
        :LimitClause form
        :OffsetClause form
        :ValuesClause form
        :Update form
        :Update1 form
        :Load form
        :Clear form
        :Drop form
        :Create form
        :Add form
        :Move form
        :Copy form
        :InsertData form
        :DeleteData form
        :DeleteWhere form
        :Modify form
        :DeleteClause form
        :InsertClause form
        :UsingClause form
        :GraphOrDefault form
        :GraphRef form
        :GraphRefAll form
        :QuadPattern form
        :QuadData form
        :Quads form
        :QuadsNotTriples form
        :TriplesTemplate form
        :GraphPatternNotTriples form
        :OptionalGraphPattern form
        :GraphGraphPattern form
        :ServiceGraphPattern form
        :Bind form
        :InlineData form
        :DataBlock form
        :InlineDataOneVar form
        :InlineDataFull form
        :DataBlockValue form
        :MinusGraphPattern form
        :GroupOrUnionGraphPattern form
        :Filter form
        :Constraint form
        :FunctionCall form
        :ArgList form
        :ExpressionList form
        :ConstructTemplate form
        :ConstructTriples form
        :TriplesSameSubject form
        :PropertyList form
        :PropertyListNotEmpty form
        :Verb form
        :ObjectList form
        :Object form
        :Integer form
        :TriplesNode form
        :BlankNodePropertyList form
        :TriplesNodePath form
        :BlankNodePropertyListPat form
        :Collection form
        :CollectionPath form
        :GraphNode form
        :VarOrIri form
        :GraphTerm form
        :Expression form
        :ConditionalOrExpression form
        :ConditionalAndExpression form
        :ValueLogical form
        :RelationalExpression form
        :NumericExpression form
        :AdditiveExpression form
        :MultiplicativeExpression form
        :UnaryExpression form
        :PrimaryExpression form
        :BrackettedExpression form
        :BuiltInCall form
        :RegexExpression form
        :SubstringExpression form
        :StrReplaceExpression form
        :ExistsFunc form
        :NotExistsFunc form
        :SolutionModifier form
        :Aggregate form
        :iriOrFunction form
        :RDFLiteral form
        :NumericLiteral form
        :NumericLiteralUnsigned form
        :NumericLiteralPositive form
        :NumericLiteralNegative form
        :BooleanLiteral form
        :String form
        :BlankNode form
        (do
          (println "Unexpected tag: " t)
          form)))
    :default form
    )
  )

(defn sparql-tx
  [form]
  (cond
    (vector? form)
    (let [[t & [v :as args]] form]
      (case t
        :PN_CHARS_BASE v
        :PN_CHARS_U v
        :VARNAME (apply str args)
        :IRIREF (uri (apply str (rest (butlast args))))

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
                       (uri? v) v
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

