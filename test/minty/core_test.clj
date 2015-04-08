(ns minty.core-test
  (:require [minty.core   :as core]
            [clojure.test :refer :all]))

(deftest no-type-annotations
  (testing "it works"
    (is (= (core/parse-body '([a b]
                              (+ a b)))
           {:args-vec    '[a b]
            :doc-string  false
            :return-type false
            :typed-defn? false}))))

(deftest type-annotations
  (testing "it works"
    (is (= (core/parse-body '([a :- Int b :- Int]
                              (+ a b)))
           {:args-vec    '[a :- Int b :- Int]
            :doc-string  false
            :return-type false
            :typed-defn? true}))
    (is (= (core/parse-body '("this is a doc string"
                              [a :- Int b :- Int] :- Int
                              (+ a b)))
           {:args-vec    '[a :- Int b :- Int]
            :doc-string  "this is a doc string"
            :return-type 'Int
            :typed-defn? true}))))
