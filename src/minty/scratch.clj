(ns minty.scratch
  (:refer-clojure :exclude [defn])
  (:require [clojure.core.typed :as t]
            [minty.core         :as minty :refer [defn]]))

(defn add-two
  "Note that the arguments are annotated"
  [a :- t/Int]
  (+ a 2))

(defn subtract-four
  "Note that the arguments and return type are annotated."
  [a :- t/Int] :- t/Int
  (- a 4))

(defn add-three
  "Note that this is unannotated"
  [a]
  (+ a 3))

#_(defn add-three-and-then-subtract-four
    "This does not type check, because add-three is not annotated."
    [a :- t/Int] :- t/Int
    (-> a
        add-three
        subtract-four))

(t/check-ns)
