(ns aoc2017.day-2
  (:import [java.io BufferedReader StringReader])
  (:require [aoc2017.helpers :as helpers]
            [clojure.string :as str]))

(defn- multiple-lines->split-digit-seq
  [input]
  (->> (map str/trim (line-seq (BufferedReader. (StringReader. input))))
       (map #(str/split % #"\s+"))
       (map #(map helpers/parse-int %))))

(defn solve-part-1
  [input]
  (let [to-digits (multiple-lines->split-digit-seq input)
        mins (map #(reduce min %) to-digits)
        maxs (map #(reduce max %) to-digits)]
    (->> (map vector maxs mins)
         (map #(apply - %))
         (apply +))))
