(ns aoc2019.day-2
  (:require [clojure.string :as string]))

(defn statement
  [pic acc fn]
  (let [[_ a b r] (drop pic acc)
        a-val (nth acc a)
        b-val (nth acc b)
        result (fn a-val b-val)]
    (assoc acc r result)))

(defn add-statement
  [pic acc]
  (statement pic acc +))

(defn multiply-statement
  [pic acc]
  (statement pic acc *))

(defn run-program
  [data]
  (let [output (loop [pic 0
                      result data]
                 (let [operation (first (drop pic result))
                       [statement-result operations-consumed] (cond
                                                                (= 1 operation) (list (add-statement pic result) 4)
                                                                (= 2 operation) (list (multiply-statement pic result) 4)
                                                                (= 99 operation) (list result 1))
                       next-pic (+ pic operations-consumed)]
                   (if (= 99 operation)
                     statement-result
                     (recur next-pic statement-result))))]
    (first output)))

(defn loader
  []
  (as-> (slurp "resources/day2.txt") s
    (string/trim s)
    (string/split s #"\,")
    (map #(Integer/parseInt %) s)
    (into [] s))) ;; Force realization of the lazy sequence

(def program (loader))

(defn find-inputs
  []
  (let [options (for [noun (range 0 100)
                      verb (range 0 100)]
                  [noun verb])
        result (reduce (fn [acc x]
                         (let [noun (first x)
                               verb (second x)
                               output (-> program
                                          (assoc 1 noun)
                                          (assoc 2 verb)
                                          run-program)]
                           (if (= 19690720 output)
                             {:noun noun
                              :verb verb}
                             acc)))
                       {} options)]
    (+ (* 100 (:noun result)) (:verb result))))

(defn run
  []
  (let [part-1 (-> (loader)
                   (assoc 1 12)
                   (assoc 2 2)
                   run-program)
        part-2 (find-inputs)]
    (prn "2.1" part-1)
    (prn "2.2" part-2)))

(comment
  (run)

  (find-inputs)
  )

(for [x [1 2 3]
      y '(a b c)]
  [x y])
