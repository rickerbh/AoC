(ns aoc2019.day-2-test
  (:require [aoc2019.day-2 :as sut]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest part-1-test
  (testing "Examples for part 1 work"
    (is (= [2 0 0 0 99] (sut/run-program [1 0 0 0 99])))
    (is (= [2,3,0,6,99] (sut/run-program [2,3,0,3,99])))
    (is (= [2,4,4,5,99,9801] (sut/run-program [2,4,4,5,99,0])))
    (is (= [30,1,1,4,2,5,6,0,99] (sut/run-program [1,1,1,4,99,5,6,0,99])))))
