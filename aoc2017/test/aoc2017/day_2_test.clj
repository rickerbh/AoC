(ns aoc2017.day-2-test
  (:require [clojure.test :refer :all]
            [aoc2017.day-2 :refer :all]))

(def input
  "5 1 9 5
  7 5 3
  2 4 6 8")

(deftest part-1-tests
  (testing "Examples for part 1 work"
    (is (= 18 (solve-part-1 input)))))
