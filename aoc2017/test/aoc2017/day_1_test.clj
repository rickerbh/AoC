(ns aoc2017.day-1-test
  (:require [clojure.test :refer :all]
            [aoc2017.day-1 :refer :all]))

(deftest part-1-tests
  (testing "Examples for part 1 work"
    (is (= 3 (solve 1122)))
    (is (= 4 (solve 1111)))
    (is (= 0 (solve 1234)))
    (is (= 9 (solve 91212129)))))

(deftest part-2-tests
  (testing "Examples for part 2 work"
    (is (= 6 (solve-part-2 1212)))
    (is (= 0 (solve-part-2 1221)))
    (is (= 4 (solve-part-2 123425)))
    (is (= 12 (solve-part-2 123123)))
    (is (= 4 (solve-part-2 12131415)))))
