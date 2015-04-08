(ns minty.core
  (:require [clojure.core.typed :as t]))

(t/defn ^:no-check typed-args-vec?
  [args-vec]
  (let [partitioned (partition 3 args-vec)]
    (and (not (empty? partitioned))
         (every? (fn [[v something alleged-type]]
                   (= something :-))
                 partitioned))))

(t/defn ^:no-check parse-body
  [body]
  (let [doc-string  (and (string? (first body))
                         (first body))
        args-vec    (if doc-string
                      (and (vector? (second body))
                           (second body))
                      (and (vector? (first body))
                           (first body)))
        return-type (if doc-string
                      (and (> (count body) 3)
                           (= (nth body 2) :-)
                           (nth body 3))
                      (and (> (count body) 2)
                           (= (nth body 1) :-)
                           (nth body 2)))
        typed-defn? (typed-args-vec? args-vec)]
    (if-not args-vec
      :error
      {:args-vec    args-vec
       :doc-string  doc-string
       :return-type return-type
       :typed-defn? typed-defn?})))

(defmacro defn
  [fn-name & body]
  (let [{:keys [typed-defn? args-vec return-type fn-body] :as parse-result} (parse-body body)]
    (if typed-defn?
      `(t/defn ~fn-name ~@body)
      `(t/defn ~(with-meta fn-name {:no-check true}) ~@body))))
