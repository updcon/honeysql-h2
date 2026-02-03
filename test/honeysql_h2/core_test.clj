(ns honeysql-h2.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [honey.sql :as sql]
            [honeysql-h2.core :as h2]))

;; =============================================================================
;; boolean! tests
;; =============================================================================

(deftest boolean!-nil-test
  (testing "nil coerces to false"
    (is (false? (h2/boolean! nil)))))

(deftest boolean!-boolean-passthrough-test
  (testing "booleans pass through unchanged"
    (is (true? (h2/boolean! true)))
    (is (false? (h2/boolean! false)))))

(deftest boolean!-truthy-strings-test
  (testing "truthy string values"
    (are [input] (true? (h2/boolean! input))
      "true"   "TRUE"   "True"   "TrUe"
      "yes"    "YES"    "Yes"
      "on"     "ON"     "On"
      "1"
      "t"      "T"
      "y"      "Y")))

(deftest boolean!-falsy-strings-test
  (testing "falsy string values"
    (are [input] (false? (h2/boolean! input))
      "false"  "FALSE"  "False"  "FaLsE"
      "no"     "NO"     "No"
      "off"    "OFF"    "Off"
      "0"
      "f"      "F"
      "n"      "N"
      "")))

(deftest boolean!-whitespace-strings-test
  (testing "whitespace-only strings are falsy"
    (are [input] (false? (h2/boolean! input))
      " "
      "  "
      "\t"
      "\n"
      "  \t\n  ")))

(deftest boolean!-trimmed-strings-test
  (testing "strings with surrounding whitespace are trimmed"
    (is (true? (h2/boolean! "  true  ")))
    (is (true? (h2/boolean! "\tyes\n")))
    (is (false? (h2/boolean! "  false  ")))
    (is (false? (h2/boolean! "\tno\n")))))

(deftest boolean!-unknown-strings-test
  (testing "unknown strings default to false (strict mode)"
    (are [input] (false? (h2/boolean! input))
      "maybe"
      "unknown"
      "2"
      "-1"
      "truthy"
      "falsy"
      "enabled"
      "disabled")))

(deftest boolean!-positive-numbers-test
  (testing "positive numbers are truthy"
    (are [input] (true? (h2/boolean! input))
      1
      42
      0.1
      0.001
      100
      Long/MAX_VALUE
      1.5
      1/2)))

(deftest boolean!-zero-and-negative-numbers-test
  (testing "zero and negative numbers are falsy"
    (are [input] (false? (h2/boolean! input))
      0
      0.0
      -1
      -42
      -0.5
      Long/MIN_VALUE)))

(deftest boolean!-collections-test
  (testing "non-empty collections are truthy"
    (are [input] (true? (h2/boolean! input))
      [1]
      [1 2 3]
      '(1)
      #{1}
      {:a 1}))

  (testing "empty collections are falsy"
    (are [input] (false? (h2/boolean! input))
      []
      '()
      #{}
      {})))

(deftest boolean!-other-types-test
  (testing "other non-nil values are truthy"
    (are [input] (true? (h2/boolean! input))
      :keyword
      'symbol
      #"regex"
      (Object.)
      (java.util.Date.))))

;; =============================================================================
;; fbool tests
;; =============================================================================

(deftest fbool-test
  (testing "fbool extracts first element and converts to boolean"
    (is (true? (h2/fbool ["true"])))
    (is (true? (h2/fbool [true])))
    (is (true? (h2/fbool [1])))
    (is (false? (h2/fbool ["false"])))
    (is (false? (h2/fbool [false])))
    (is (false? (h2/fbool [0])))
    (is (false? (h2/fbool [nil])))
    (is (false? (h2/fbool []))))

  (testing "fbool works with query result sets (vectors of maps)"
    ;; Simulating a JDBC result like [{:exists true}]
    (is (true? (h2/fbool [{:exists true}])))
    (is (false? (h2/fbool [])))))

;; =============================================================================
;; upsert tests
;; =============================================================================

(deftest upsert-basic-test
  (testing "basic upsert without primary key"
    (let [[sql & params] (h2/upsert [{:aa 11 :bb 22}] :table)]
      (is (string? sql))
      (is (clojure.string/includes? sql "MERGE INTO"))
      (is (clojure.string/includes? sql "VALUES"))
      (is (= [11 22] params)))))

(deftest upsert-with-single-key-test
  (testing "upsert with single primary key"
    (let [[sql & params] (h2/upsert [{:aa 11 :bb 22}] :table :aa)]
      (is (clojure.string/includes? sql "KEY(aa)"))
      (is (= [11 22] params)))))

(deftest upsert-with-composite-key-test
  (testing "upsert with composite primary key"
    (let [[sql & params] (h2/upsert [{:aa 11 :bb 22}] :table [:p :s])]
      (is (clojure.string/includes? sql "KEY(p,s)"))
      (is (= [11 22] params)))))

(deftest upsert-multiple-records-test
  (testing "upsert with multiple records"
    (let [[sql & params] (h2/upsert [{:a 1 :b 2} {:a 3 :b 4}] :table)]
      (is (clojure.string/includes? sql "MERGE INTO"))
      (is (= [1 2 3 4] params)))))

;; =============================================================================
;; timestamp tests
;; =============================================================================

(deftest timestamp-no-args-test
  (testing "timestamp without args returns current time"
    (let [before (System/currentTimeMillis)
          ^java.sql.Timestamp ts (h2/timestamp)
          after (System/currentTimeMillis)]
      (is (instance? java.sql.Timestamp ts))
      (is (<= before (.getTime ts) after)))))

(deftest timestamp-with-millis-test
  (testing "timestamp with millis argument"
    (let [millis 1234567890000
          ^java.sql.Timestamp ts (h2/timestamp millis)]
      (is (instance? java.sql.Timestamp ts))
      (is (= millis (.getTime ts))))))

;; =============================================================================
;; LISTAGG tests
;; =============================================================================

(deftest listagg-basic-test
  (testing "basic LISTAGG with expression only"
    (let [[sql] (sql/format {:select [[[:listagg :name] :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(name)"))))

  (testing "LISTAGG with separator"
    (let [[sql] (sql/format {:select [[[:listagg :name ", "] :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(name, ', ')")))))

(deftest listagg-distinct-test
  (testing "LISTAGG DISTINCT"
    (let [[sql] (sql/format {:select [[[:listagg-distinct :name ", "] :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(DISTINCT name, ', ')")))))

(deftest listagg-on-overflow-error-test
  (testing "LISTAGG with ON OVERFLOW ERROR"
    (let [[sql] (sql/format {:select [[[:listagg :name ", " :error] :names]]})]
      (is (clojure.string/includes? sql "ON OVERFLOW ERROR")))))

(deftest listagg-on-overflow-truncate-test
  (testing "LISTAGG with ON OVERFLOW TRUNCATE (no filler)"
    (let [[sql] (sql/format {:select [[[:listagg :name ", " [:truncate]] :names]]})]
      (is (clojure.string/includes? sql "ON OVERFLOW TRUNCATE"))))

  (testing "LISTAGG with ON OVERFLOW TRUNCATE with filler"
    (let [[sql] (sql/format {:select [[[:listagg :name ", " [:truncate "..."]] :names]]})]
      (is (clojure.string/includes? sql "ON OVERFLOW TRUNCATE"))
      (is (clojure.string/includes? sql "..."))))

  (testing "LISTAGG with ON OVERFLOW TRUNCATE WITH COUNT"
    (let [[sql] (sql/format {:select [[[:listagg :name ", " [:truncate "..." :with-count]] :names]]})]
      (is (clojure.string/includes? sql "ON OVERFLOW TRUNCATE"))
      (is (clojure.string/includes? sql "WITH COUNT"))))

  (testing "LISTAGG with ON OVERFLOW TRUNCATE WITHOUT COUNT"
    (let [[sql] (sql/format {:select [[[:listagg :name ", " [:truncate "..." :without-count]] :names]]})]
      (is (clojure.string/includes? sql "ON OVERFLOW TRUNCATE"))
      (is (clojure.string/includes? sql "WITHOUT COUNT")))))

(deftest listagg-within-group-test
  (testing "LISTAGG with WITHIN GROUP"
    (let [[sql] (sql/format {:select [[[:within-group
                                        [:listagg :name ", "]
                                        {:order-by [:name]}]
                                       :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(name, ', ')"))
      (is (clojure.string/includes? sql "WITHIN GROUP"))
      (is (clojure.string/includes? sql "ORDER BY")))))

(deftest listagg-filter-test
  (testing "LISTAGG with FILTER"
    (let [[sql & params] (sql/format {:select [[[:filter
                                                 [:listagg :name ", "]
                                                 {:where [:= :active true]}]
                                                :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(name, ', ')"))
      (is (clojure.string/includes? sql "FILTER"))
      (is (clojure.string/includes? sql "WHERE")))))

(deftest listagg-over-test
  (testing "LISTAGG with OVER (window function)"
    ;; HoneySQL expects :over to contain a vector of [expr window-spec] pairs
    (let [[sql] (sql/format {:select [[[:over
                                        [[[:within-group
                                           [:listagg :name ", "]
                                           {:order-by [:name]}]
                                          {:partition-by [:department]}]]]
                                       :names]]})]
      (is (clojure.string/includes? sql "LISTAGG(name, ', ')"))
      (is (clojure.string/includes? sql "WITHIN GROUP"))
      (is (clojure.string/includes? sql "OVER"))
      (is (clojure.string/includes? sql "PARTITION BY")))))

;; =============================================================================
;; listagg helper function tests
;; =============================================================================

(deftest listagg-helper-basic-test
  (testing "listagg helper - basic"
    (is (= [:listagg :name] (h2/listagg :name))))

  (testing "listagg helper - with separator"
    (is (= [:listagg :name ", "] (h2/listagg :name :separator ", "))))

  (testing "listagg helper - with distinct"
    (is (= [:listagg-distinct :name ", "]
           (h2/listagg :name :separator ", " :distinct true)))))

(deftest listagg-helper-on-overflow-test
  (testing "listagg helper - on-overflow error"
    (is (= [:listagg :name ", " :error]
           (h2/listagg :name :separator ", " :on-overflow :error))))

  (testing "listagg helper - on-overflow truncate"
    (is (= [:listagg :name ", " [:truncate "..." :with-count]]
           (h2/listagg :name :separator ", " :on-overflow [:truncate "..." :with-count])))))

(deftest listagg-helper-within-group-test
  (testing "listagg helper - with within-group"
    (is (= [:within-group [:listagg :name ", "] {:order-by [:name]}]
           (h2/listagg :name :separator ", " :within-group {:order-by [:name]})))))

(deftest listagg-helper-filter-test
  (testing "listagg helper - with filter"
    (is (= [:filter [:listagg :name ", "] {:where [:= :active true]}]
           (h2/listagg :name :separator ", " :filter [:= :active true])))))

(deftest listagg-helper-combined-test
  (testing "listagg helper - all options combined"
    (let [result (h2/listagg :name
                             :separator ", "
                             :distinct true
                             :on-overflow [:truncate "..." :with-count]
                             :within-group {:order-by [:name]}
                             :filter [:= :active true]
                             :over {:partition-by [:dept]})]
      ;; Should be: [:over [:filter [:within-group [:listagg-distinct :name ", " [...]] {...}] {...}] {...}]
      (is (= :over (first result)))
      (is (vector? result))))

  (testing "listagg helper - full SQL generation"
    (let [expr (h2/listagg :name
                           :separator ", "
                           :distinct true
                           :within-group {:order-by [:name]}
                           :filter [:= :active true])
          [sql & _] (sql/format {:select [[expr :names]] :from [:users]})]
      (is (clojure.string/includes? sql "LISTAGG(DISTINCT name, ', ')"))
      (is (clojure.string/includes? sql "WITHIN GROUP"))
      (is (clojure.string/includes? sql "ORDER BY"))
      (is (clojure.string/includes? sql "FILTER"))
      (is (clojure.string/includes? sql "WHERE")))))
