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

;; -----------------------------------------------------------------------------
;; SEQUENCE support
;; H2 syntax:
;; CREATE SEQUENCE [IF NOT EXISTS] name [AS type] [START WITH n] [INCREMENT BY n]
;;   [MINVALUE n | NO MINVALUE] [MAXVALUE n | NO MAXVALUE] [CYCLE | NO CYCLE] [CACHE n | NO CACHE]
;; ALTER SEQUENCE [IF EXISTS] name [RESTART [WITH n]] [INCREMENT BY n] ...
;; DROP SEQUENCE [IF EXISTS] name
;; NEXT VALUE FOR sequence_name
;; CURRENT VALUE FOR sequence_name
;; -----------------------------------------------------------------------------

(defn- format-sequence-options
  "Formats sequence options into SQL string.
   Returns a string with all the option clauses."
  [opts]
  (let [parts (cond-> []
                (:as opts)
                (conj (str "AS " (sql/sql-kw (:as opts))))

                (:start-with opts)
                (conj (str "START WITH " (:start-with opts)))

                (:restart opts)
                (conj "RESTART")

                (:restart-with opts)
                (conj (str "RESTART WITH " (:restart-with opts)))

                (:increment-by opts)
                (conj (str "INCREMENT BY " (:increment-by opts)))

                (:minvalue opts)
                (conj (str "MINVALUE " (:minvalue opts)))

                (:no-minvalue opts)
                (conj "NO MINVALUE")

                (:maxvalue opts)
                (conj (str "MAXVALUE " (:maxvalue opts)))

                (:no-maxvalue opts)
                (conj "NO MAXVALUE")

                (:cycle opts)
                (conj "CYCLE")

                (:no-cycle opts)
                (conj "NO CYCLE")

                (:cache opts)
                (conj (str "CACHE " (:cache opts)))

                (:no-cache opts)
                (conj "NO CACHE"))]
    (s/join " " parts)))

(defn- create-sequence-formatter
  "Formatter for CREATE SEQUENCE clause.
   Accepts:
   - keyword/symbol: sequence name
   - [name opts-map]: sequence name with options"
  [_ x]
  (let [[seq-name opts] (if (sequential? x) x [x {}])
        [name-sql] (sql/format-expr seq-name)
        opts-str (format-sequence-options (dissoc opts :if-not-exists))]
    [(str "CREATE SEQUENCE "
          (when (:if-not-exists opts) "IF NOT EXISTS ")
          name-sql
          (when (seq opts-str) (str " " opts-str)))]))

(defn- alter-sequence-formatter
  "Formatter for ALTER SEQUENCE clause.
   Accepts:
   - keyword/symbol: sequence name
   - [name opts-map]: sequence name with options"
  [_ x]
  (let [[seq-name opts] (if (sequential? x) x [x {}])
        [name-sql] (sql/format-expr seq-name)
        opts-str (format-sequence-options (dissoc opts :if-exists))]
    [(str "ALTER SEQUENCE "
          (when (:if-exists opts) "IF EXISTS ")
          name-sql
          (when (seq opts-str) (str " " opts-str)))]))

(defn- drop-sequence-formatter
  "Formatter for DROP SEQUENCE clause.
   Accepts:
   - keyword/symbol: sequence name
   - [name opts-map]: sequence name with options (if-exists)"
  [_ x]
  (let [[seq-name opts] (if (sequential? x) x [x {}])
        [name-sql] (sql/format-expr seq-name)]
    [(str "DROP SEQUENCE "
          (when (:if-exists opts) "IF EXISTS ")
          name-sql)]))

(defn- next-value-for-formatter
  "Formatter for NEXT VALUE FOR function."
  [_ args]
  (let [[seq-name] args
        [name-sql] (sql/format-expr seq-name)]
    [(str "NEXT VALUE FOR " name-sql)]))

(defn- current-value-for-formatter
  "Formatter for CURRENT VALUE FOR function."
  [_ args]
  (let [[seq-name] args
        [name-sql] (sql/format-expr seq-name)]
    [(str "CURRENT VALUE FOR " name-sql)]))

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

    ;; Register SEQUENCE functions
    (sql/register-fn! :next-value-for next-value-for-formatter)
    (sql/register-fn! :current-value-for current-value-for-formatter)

    ;; Register SEQUENCE clauses (order matters: later in list = earlier in SQL)
    ;; These are standalone statements, not part of SELECT queries
    (sql/register-clause! :create-sequence create-sequence-formatter nil)
    (sql/register-clause! :alter-sequence alter-sequence-formatter nil)
    (sql/register-clause! :drop-sequence drop-sequence-formatter nil)

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

;; -----------------------------------------------------------------------------
;; SEQUENCE helper functions
;; -----------------------------------------------------------------------------

(defn create-sequence
  "Constructs a CREATE SEQUENCE statement map for H2.

   Arguments:
   - seq-name: The sequence name (keyword or symbol)
   - opts: Optional map of sequence options

   Options:
   - :if-not-exists - Add IF NOT EXISTS clause
   - :as - Data type (e.g., :bigint, :int)
   - :start-with - Starting value
   - :increment-by - Increment value
   - :minvalue - Minimum value
   - :no-minvalue - Use NO MINVALUE
   - :maxvalue - Maximum value
   - :no-maxvalue - Use NO MAXVALUE
   - :cycle - Enable cycling
   - :no-cycle - Disable cycling
   - :cache - Cache size
   - :no-cache - Disable caching

   Examples:
   (create-sequence :my_seq)
   ;; => {:create-sequence :my_seq}

   (create-sequence :my_seq {:start-with 100 :increment-by 10})
   ;; => {:create-sequence [:my_seq {:start-with 100 :increment-by 10}]}"
  ([seq-name]
   {:create-sequence seq-name})
  ([seq-name opts]
   {:create-sequence [seq-name opts]}))

(defn alter-sequence
  "Constructs an ALTER SEQUENCE statement map for H2.

   Arguments:
   - seq-name: The sequence name (keyword or symbol)
   - opts: Map of alter options

   Options:
   - :if-exists - Add IF EXISTS clause
   - :restart - Restart the sequence
   - :restart-with - Restart with specific value
   - :increment-by - Change increment value
   - :minvalue/:no-minvalue - Change minimum
   - :maxvalue/:no-maxvalue - Change maximum
   - :cycle/:no-cycle - Change cycling
   - :cache/:no-cache - Change caching

   Examples:
   (alter-sequence :my_seq {:restart-with 500})
   ;; => {:alter-sequence [:my_seq {:restart-with 500}]}"
  [seq-name opts]
  {:alter-sequence [seq-name opts]})

(defn drop-sequence
  "Constructs a DROP SEQUENCE statement map for H2.

   Arguments:
   - seq-name: The sequence name (keyword or symbol)
   - opts: Optional map with :if-exists

   Examples:
   (drop-sequence :my_seq)
   ;; => {:drop-sequence :my_seq}

   (drop-sequence :my_seq {:if-exists true})
   ;; => {:drop-sequence [:my_seq {:if-exists true}]}"
  ([seq-name]
   {:drop-sequence seq-name})
  ([seq-name opts]
   {:drop-sequence [seq-name opts]}))

(defn nextval
  "Constructs a NEXT VALUE FOR expression for use in queries.

   Arguments:
   - seq-name: The sequence name (keyword or symbol)

   Examples:
   (nextval :my_seq)
   ;; => [:next-value-for :my_seq]

   ;; In a query:
   {:select [[(nextval :my_seq) :id]]}
   ;; => SELECT NEXT VALUE FOR my_seq AS id"
  [seq-name]
  [:next-value-for seq-name])

(defn currval
  "Constructs a CURRENT VALUE FOR expression for use in queries.

   Arguments:
   - seq-name: The sequence name (keyword or symbol)

   Examples:
   (currval :my_seq)
   ;; => [:current-value-for :my_seq]

   ;; In a query:
   {:select [[(currval :my_seq) :current_id]]}
   ;; => SELECT CURRENT VALUE FOR my_seq AS current_id"
  [seq-name]
  [:current-value-for seq-name])
