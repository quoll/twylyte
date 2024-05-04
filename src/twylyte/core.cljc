(ns twylyte.core
  (:require [clojure.walk :as walk]
            [quoll.rdf :as rdf]
            #?(:clj  [instaparse.core :refer [parser parses defparser]]
               :cljs [instaparse.core :refer [parser parses] :refer-macros [defparser]]))
  #?(:clj (:import [java.net URI])))

(def ^:const grammar-path "resources/sparql.grammar")

(defparser sparql-parse grammar-path :output-format :hiccup :auto-whitespace :standard :string-ci true)

#?(:clj
   (defn uri
     [u]
     (URI. u))

   :cljs
   (defn uri
     [u]
     (goog.Uri. u)))

(defn ast
  "Converts elemtns of the CST (Concrete Syntax Tree) to an AST (Abstract Syntax Tree)"
  [form]
  (cond
    (vector? form)
    (let [[t & [v :as args]] form]
      (case t
        :PN_CHARS_BASE v
        :PN_CHARS_U v
        :VARNAME (apply str args)
        (:VAR1 :VAR2) (symbol (apply str args))
        :IRIREF (rdf/iri (apply str (rest (butlast args))))

        (:Var :VarOrTerm :GraphNodePath :ObjectPath :ObjectListPath :VerbSimple) v


        :PNAME_NS form
        :PNAME_LN form
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
        :PN_CHARS form
        :PN_PREFIX form
        :PN_LOCAL form
        :PLX form
        :PERCENT form
        :HEX form
        :PN_LOCAL_ESC form
        :UpdateUnit form
        :BaseDecl form
        :PrefixDecl form
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
        :VerbSimple form
        :PathMod form
        :PathNegatedPropertySet form
        :PathOneInPropertySet form
        :Integer form
        :TriplesNode form
        :BlankNodePropertyList form
        :TriplesNodePath form
        :BlankNodePropertyListPat form
        :Collection form
        :CollectionPath form
        :GraphNode form
        :VarOrIri form
        :Var form
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
        :Aggregate form
        :iriOrFunction form
        :RDFLiteral form
        :NumericLiteral form
        :NumericLiteralUnsigned form
        :NumericLiteralPositive form
        :NumericLiteralNegative form
        :BooleanLiteral form
        :String form
        :PrefixedName form
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

