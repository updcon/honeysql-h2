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

(defn boolean! [v]
  (if (not (boolean? v))
    (boolean
      (if (java.lang.Boolean/parseBoolean (str (or v "")))
        true
        (if (= v (.compareToIgnoreCase (str (or v "")) "false"))
          false
          (if (number? v) (pos? v) false))))
    v))

(def fbool (comp boolean! first))

(defn upsert [data table & [pk]]
  (-> {:merge-into table
       :records    [pk data]}
      sql/format))

(defn timestamp
  ([] (timestamp (System/currentTimeMillis)))
  ([millis] (Timestamp. millis)))
