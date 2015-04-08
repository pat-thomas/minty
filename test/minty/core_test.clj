(ns minty.core-test
  (:require [minty.core   :as core]
            [clojure.test :refer :all]))

(deftest no-type-annotations
  (testing "it works"
    (is (= (core/parse-body '([a b]
                              (+ a b)))
           {:args-vec    '[a b]
            :return-type false
            :typed-defn? false}))))

(deftest type-annotations
  (testing "it works"
    (is (= (core/parse-body '([a :- Int b :- Int]
                              (+ a b)))
           {:args-vec    '[a :- Int b :- Int]
            :return-type false
            :typed-defn? true}))
    (is (= (core/parse-body '([a :- Int b :- Int] :- Int
                              (+ a b)))
           {:args-vec    '[a :- Int b :- Int]
            :return-type 'Int
            :typed-defn? true}))))
