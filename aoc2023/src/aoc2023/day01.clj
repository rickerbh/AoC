(ns aoc2023.day01
  (:require [clojure.string :as s]
            [clojure.test :refer :all]))

(def part-01-test-data
  "o1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defn part01
  [input]
  (->> input
       s/split-lines
       (map #(s/replace % #"[^0-9]+" ""))
       (map #(Integer/parseInt (str (first %) (last %))))
       (apply +)))

(def part-02-test-data "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def words-to-digits
  {"zero" "0"
   "one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn part02
  [input]
  (->> (loop [in input
              acc ""]
         (if (empty? in)
           acc
           (if-let [digit-word (-> (filter (fn [[k _]]
                                             (when (s/starts-with? in k)
                                               k)) words-to-digits)
                                   first
                                   first)]
             (recur (s/replace-first in digit-word "") (str acc (str (get words-to-digits digit-word))))
             (recur (apply str (drop 1 in)) (str acc (str (first in)))))))
       s/split-lines
       (map #(s/replace % #"[^0-9]+" ""))
       (map #(Integer/parseInt (str (first %) (last %))))
       (apply +)))


(comment
  (= 142 (part01 part-01-test-data))
  (= 281 (part02 part-02-test-data)))

(defn run
  []
  (prn "Day 01")
  (prn "Part 01 answer:" (part01 (slurp "resources/day01.input")))
  (prn "Part 02 answer:" (part02 (slurp "resources/day01.input"))))

(run)
