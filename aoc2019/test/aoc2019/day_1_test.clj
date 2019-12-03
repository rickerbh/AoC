(ns aoc2019.day-1-test
  (:require [aoc2019.day-1 :as sut]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest part-1-test
  (testing "Examples for part 1 work"
    (is (= 2 (sut/calc-module-fuel 12)))
    (is (= 2 (sut/calc-module-fuel 14)))
    (is (= 654 (sut/calc-module-fuel 1969)))
    (is (= 33583 (sut/calc-module-fuel 100756)))))

(deftest part-2-test
  (testing "Examples for part 2 work"
    (is (= 2 (sut/calc-module-fuel-and-fuels-fuel 14)))
    (is (= 966 (sut/calc-module-fuel-and-fuels-fuel 1969)))
    (is (= 50346 (sut/calc-module-fuel-and-fuels-fuel 100756)))))
