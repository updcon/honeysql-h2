(ns honeysql-h2.core
  (:require [clojure.java.jdbc :refer [IResultSetReadColumn]]
            [clojure.string :as s]
            [honey.sql :as sql])
  (:import (java.sql Date Time Timestamp)))

;; honeysql extensions
(defn h2ext []
  (do
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
    ;(println ">> H2 clauses registered")
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

(defn timestamp
  ([] (timestamp (System/currentTimeMillis)))
  ([millis] (Timestamp. millis)))
