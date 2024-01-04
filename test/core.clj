(ns core
  (:require [clojure.test :refer :all]
            [honeysql-h2.core :refer :all]))

;;

(deftest check-basic-constructs
  (testing "Testing sample upsert impl"
    (is (= (upsert [{:aa 11 :bb 22}] :table)
           ["MERGE INTO table (aa, bb) VALUES (?, ?)" 11 22]))
    (is (= (upsert [{:aa 11 :bb 22}] :table :aa)
           ["MERGE INTO table (aa, bb) KEY(aa) VALUES (?, ?)" 11 22]))
    (is (= (upsert [{:aa 11 :bb 22}] :table [:p :s])
           ["MERGE INTO table (aa, bb) KEY(p,s) VALUES (?, ?)" 11 22]))))
