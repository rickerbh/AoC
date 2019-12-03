(ns aoc2019.day-1
  (:require [clojure.string :as string]))

(defn calc-module-fuel
  [mass]
  (let [fuel (-> mass
                 (/ 3)
                 int
                 (- 2))]
    (if (<= 0 fuel)
      fuel
      0)))

(defn calc-module-fuel-and-fuels-fuel
  [mass]
  (loop [remaining mass
         total 0]
    (if (>= 0 remaining)
      total
      (let [required-fuel (calc-module-fuel remaining)
            new-total (+ total required-fuel)]
        (recur required-fuel
               new-total)))))

(defn loader
  []
  (string/split-lines (slurp "resources/day1.txt")))

(defn run
  []
  (let [part-1 (->> (loader)
                    (map #(-> % Integer/parseInt calc-module-fuel))
                    (reduce +))
        part-2 (->> (loader)
                    (map #(-> % Integer/parseInt calc-module-fuel-and-fuels-fuel))
                    (reduce +))]
    (prn "1.1" part-1)
    (prn "1.2" part-2)))
