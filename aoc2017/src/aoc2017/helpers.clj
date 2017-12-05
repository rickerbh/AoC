(ns aoc2017.helpers)

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       (mapv int)
       rseq))

(defn rotate [n s] 
  (let [shift (mod n (count s))] 
    (concat (drop shift s) 
            (take shift s))))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))
