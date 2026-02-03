(ns honeysql-h2.core
  (:require [clojure.java.jdbc :refer [IResultSetReadColumn]]
            [clojure.string :as s]
            [honey.sql :as sql])
  (:import (java.sql Date Time Timestamp)))

;; honeysql extensions
;; -----------------------------------------------------------------------------
;; LISTAGG function support
;; H2 syntax: LISTAGG([DISTINCT|ALL] expr [, separator] [ON OVERFLOW ...])
;;            WITHIN GROUP (ORDER BY ...) [FILTER (WHERE ...)] [OVER (...)]
;; -----------------------------------------------------------------------------

(defn- format-on-overflow
  "Formats the ON OVERFLOW clause for LISTAGG.
   Accepts:
   - :error or :on-overflow-error -> ON OVERFLOW ERROR
   - [:truncate] -> ON OVERFLOW TRUNCATE
   - [:truncate filler] -> ON OVERFLOW TRUNCATE 'filler'
   - [:truncate filler :with-count] -> ON OVERFLOW TRUNCATE 'filler' WITH COUNT
   - [:truncate filler :without-count] -> ON OVERFLOW TRUNCATE 'filler' WITHOUT COUNT"
  [overflow]
  (cond
    (or (= overflow :error) (= overflow :on-overflow-error))
    " ON OVERFLOW ERROR"

    (and (vector? overflow) (= :truncate (first overflow)))
    (let [[_ filler count-opt] overflow
          [filler-sql] (when filler (sql/format-expr [:inline filler]))]
      (str " ON OVERFLOW TRUNCATE"
           (when filler-sql (str " " filler-sql))
           (case count-opt
             :with-count " WITH COUNT"
             :without-count " WITHOUT COUNT"
             nil)))

    :else nil))

(defn- listagg-formatter
  "Formatter for LISTAGG function.
   Args can be:
   - [expr] -> LISTAGG(expr)
   - [expr separator] -> LISTAGG(expr, 'separator')
   - [expr separator overflow] -> LISTAGG(expr, 'separator' ON OVERFLOW ...)"
  [f args]
  (let [distinct? (= f :listagg-distinct)
        [expr separator overflow] args
        [expr-sql & expr-params] (sql/format-expr expr)
        [sep-sql] (when separator (sql/format-expr [:inline separator]))
        base-sql (str "LISTAGG("
                      (when distinct? "DISTINCT ")
                      expr-sql
                      (when sep-sql (str ", " sep-sql))
                      (when overflow (format-on-overflow overflow))
                      ")")]
    (into [base-sql] expr-params)))

(defn h2ext []
  (do
    ;; Register LISTAGG functions
    (sql/register-fn! :listagg listagg-formatter)
    (sql/register-fn! :listagg-distinct listagg-formatter)

    ;; Register MERGE INTO clauses
    (sql/register-clause! :records
                          (fn [_ [pk xs]]
                            (let [cols-1 (keys (first xs))
                                  cols-n (into #{} (mapcat keys) xs)
                                  cols (if (= (set cols-1) cols-n) cols-1 cols-n)
                                  [sqls params]
                                  (reduce (fn [[sql params] [sqls' params']]
                                            [(conj sql (str "(" (s/join ", " sqls') ")"))
                                             (if params' (into params params') params')])
                                          [[] []]
                                          (map (fn [m]
                                                 (sql/format-expr-list
                                                   (map #(get m %) cols)))
                                               xs))]
                              (into [(str "("
                                          (s/join ", "
                                                  (map #(sql/format-entity % {:drop-ns true}) cols))
                                          ") "
                                          (when pk (str (sql/sql-kw :key) "("
                                                        (if (sequential? pk)
                                                          (s/join "," (map #(sql/format-entity % {:drop-ns true}) pk))
                                                          (sql/format-entity pk {:drop-ns true}))
                                                        ") "))
                                          (sql/sql-kw :values)
                                          " "
                                          (s/join ", " sqls))]
                                    params)))
                          nil)

    (sql/register-clause! :merge-into
                          (fn [clause x]
                            (let [[sql & params]
                                  (if (ident? x)
                                    (sql/format-expr x)
                                    (sql/format-dsl x))]
                              (into [(str (sql/sql-kw clause) " " sql)] params)))
                          :records)                         ; <!> MERGE INTO ... ... KEY(_pk) VALUES ...
    true))

(defonce ^:static ^:private h2-inited (h2ext))
;; Protocol extensions

(extend-protocol IResultSetReadColumn
  Date
  (result-set-read-column [v _2 _3]
    (.toLocalDate v))

  Time
  (result-set-read-column [v _2 _3]
    (.toLocalTime v))

  Timestamp
  (result-set-read-column [v _2 _3]
    (.toLocalDateTime v)))

;; helpers

(def ^:private truthy-strings
  "Set of string values that should coerce to true (case-insensitive)"
  #{"true" "yes" "on" "1" "t" "y"})

(def ^:private falsy-strings
  "Set of string values that should coerce to false (case-insensitive)"
  #{"false" "no" "off" "0" "f" "n" ""})

(defn boolean!
  "Coerces a value to boolean with sophisticated type handling.

   Conversion rules:
   - Boolean: returned as-is
   - nil: returns false
   - String: case-insensitive matching
     - Truthy: \"true\", \"yes\", \"on\", \"1\", \"t\", \"y\"
     - Falsy: \"false\", \"no\", \"off\", \"0\", \"f\", \"n\", \"\" (empty)
     - Whitespace-only strings: false
     - Other strings: false (strict mode - unknown strings are falsy)
   - Number: positive numbers are true, zero and negative are false
   - Collection: non-empty is true, empty is false
   - Other: truthy if non-nil (standard Clojure truthiness)"
  [v]
  (cond
    ;; nil is always false
    (nil? v) false

    ;; booleans pass through unchanged
    (boolean? v) v

    ;; strings: normalize and check against known truthy/falsy values
    (string? v)
    (let [normalized (-> v s/trim s/lower-case)]
      (cond
        (truthy-strings normalized) true
        (falsy-strings normalized) false
        ;; unknown strings default to false (strict interpretation)
        :else false))

    ;; numbers: positive is true, zero/negative is false
    (number? v)
    (pos? v)

    ;; collections: non-empty is true
    (coll? v)
    (boolean (seq v))

    ;; fallback: standard Clojure truthiness (non-nil is true)
    :else true))

(def fbool (comp boolean! first))

(defn upsert [data table & [pk]]
  (-> {:merge-into table
       :records    [pk data]}
      sql/format))

(defn listagg
  "Constructs a LISTAGG aggregate expression for H2.

   Arguments:
   - expr: The expression to aggregate (required)

   Options (keyword arguments):
   - :separator    - String separator between values (default: none)
   - :distinct     - If true, use DISTINCT (default: false)
   - :on-overflow  - Overflow handling:
                     - :error -> ON OVERFLOW ERROR
                     - [:truncate] -> ON OVERFLOW TRUNCATE
                     - [:truncate \"...\"] -> ON OVERFLOW TRUNCATE '...'
                     - [:truncate \"...\" :with-count] -> ON OVERFLOW TRUNCATE '...' WITH COUNT
                     - [:truncate \"...\" :without-count] -> ON OVERFLOW TRUNCATE '...' WITHOUT COUNT
   - :within-group - Order specification map, e.g., {:order-by [:name]}
   - :filter       - Filter condition, e.g., [:= :active true]
   - :over         - Window specification, e.g., {:partition-by [:dept]}
                     Uses HoneySQL syntax: [:over [[expr window-spec]]]

   Examples:
   (listagg :name)
   ;; => [:listagg :name]

   (listagg :name :separator \", \" :distinct true)
   ;; => [:listagg-distinct :name \", \"]

   (listagg :name :separator \", \" :within-group {:order-by [:name]})
   ;; => [:within-group [:listagg :name \", \"] {:order-by [:name]}]

   (listagg :name
            :separator \", \"
            :on-overflow [:truncate \"...\" :with-count]
            :within-group {:order-by [:name]}
            :filter [:= :active true])
   ;; Full expression with all clauses"
  [expr & {:keys [separator distinct on-overflow within-group filter over]}]
  (let [;; Build the core LISTAGG expression
        fn-name (if distinct :listagg-distinct :listagg)
        core-expr (cond-> [fn-name expr]
                    separator (conj separator)
                    on-overflow (conj on-overflow))

        ;; Wrap with WITHIN GROUP if specified
        with-within-group (if within-group
                            [:within-group core-expr within-group]
                            core-expr)

        ;; Wrap with FILTER if specified
        with-filter (if filter
                      [:filter with-within-group {:where filter}]
                      with-within-group)

        ;; Wrap with OVER if specified (HoneySQL expects [:over [[expr spec]]])
        with-over (if over
                    [:over [[with-filter over]]]
                    with-filter)]

    with-over))

(defn timestamp
  ([] (timestamp (System/currentTimeMillis)))
  ([millis] (Timestamp. millis)))
