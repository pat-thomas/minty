(ns minty.core
  (:require [clojure.core.typed :as t]
            [minty.check :as check]))

(t/defn ^:no-check typed-args-vec?
  [args-vec]
  (let [partitioned (partition 3 args-vec)]
    (and (not (empty? partitioned))
         (every? (fn [[v something alleged-type]]
                   (= something :-))
                 partitioned))))

(t/defn ^:no-check parse-body
  [body]
  (let [args-vec    (and (vector? (first body))
                         (first body))
        return-type (and (> (count body) 2)
                         (= (nth body 1) :-)
                         (nth body 2))
        typed-defn? (typed-args-vec? args-vec)]
    (if-not args-vec
      :error
      {:args-vec    args-vec
       :return-type return-type
       :typed-defn? typed-defn?})))

(t/defn ^:no-check try-check-ns
  []
  (try
    (t/check-ns)
    (catch Exception ex
      ex)))

(defmacro type-check
  ([]
   `(type-check ~*ns*))
  ([target-ns]
   (let [unannotated       (->> target-ns
                                ns-publics
                                vals
                                (map meta)
                                (filter #(= (:no-check %) true))
                                (map :name)
                                sort)
         type-check-result (try-check-ns)]
     (if-not (= type-check-result :ok)
       (throw type-check-result)
       {:unannotated unannotated}))))

(defmacro defn
  [fn-name & body]
  (let [{:keys [typed-defn? args-vec return-type fn-body] :as parse-result} (parse-body body)]
    (if typed-defn?
      `(t/defn ~fn-name ~@body)
      `(t/defn ~(with-meta fn-name {:no-check true}) ~fn-name ~@body))))
