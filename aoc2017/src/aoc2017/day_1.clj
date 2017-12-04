(ns aoc2017.day-1
  (:require [aoc2017.helpers :as helpers]))

(defn- sum-matched
  [xs]
  (->> xs
       (filter #(apply = %))
       (map first)
       (apply +)))

(defn solve
  [input]
  (let [ds (helpers/digits input)
        full-vec (conj (into [] ds) (first ds))]
    (->> full-vec 
         (partition 2 1)
         (sum-matched))))

(defn solve-part-2
  [input]
  (let [ds (helpers/digits input)
        half-count (/ (count ds) 2)
        full (map vector ds (helpers/rotate half-count ds))]
    (sum-matched full)))
