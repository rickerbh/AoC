(ns aoc2019.day-3
  (:require [clojure.set :as clj-set]
            [clojure.string :as string]))

(defn direction
  [move]
  (let [d (subs move 0 1)]
    (cond (= "U" d) :up
          (= "D" d) :down
          (= "L" d) :left
          (= "R" d) :right)))

(defn change-coord-fn
  [dir]
  (cond (= :up dir) :add
        (= :down dir) :sub
        (= :left dir) :sub
        (= :right dir) :add))

(defn distance
  [move]
  (-> move
      (subs 1)
      Integer/parseInt))

(defn path
  [dir start dist]
  (let [change-dir (change-coord-fn dir)]
    (if (= :add change-dir)
      (range start (+ (inc start) dist))
      (reverse (range (- start dist) (inc start))))))

(defn points
  [move x y]
  (let [dir (direction move)
        dist (distance move)]
    (if (or (= :up dir) (= :down dir))
      (let [p (path dir y dist)]
        (map vector (repeat x) p))
      (let [p (path dir x dist)]
        (map vector p (repeat y))))))

(defn next-path
  [move x y]
  (let [p (points move x y)]
    {:start-x x
     :start-y y
     :move move
     :points (drop 1 p)
     :end-x (first (last p))
     :end-y (last (last p))}))

(defn calc-wire-path
  [wire]
  (reduce (fn
            [acc move]
            (if (empty? acc)
              (conj acc (next-path move 0 0))
              (let [pos (last acc)
                    x (:end-x pos)
                    y (:end-y pos)]
                (conj acc (next-path move x y))))) [] wire))

(defn extract-points
  [state]
  (->> state
       (map :points)
       (apply concat)))

(defn unique-points
  [state]
  (-> state
      extract-points
      set))

(defn calc-distance
  [[wire-one wire-two]]
  (let [w1-state (calc-wire-path wire-one)
        w2-state (calc-wire-path wire-two)
        w1-points (unique-points w1-state)
        w2-points (unique-points w2-state)
        common-points (clj-set/intersection w1-points w2-points)
        abs-points (map (fn [xs] (map #(. java.lang.Math abs %) xs)) common-points)
        distance (map #(apply + %) abs-points)]
    (apply min distance)))

(defn find-index
  [x xs]
  (let [result (reduce (fn [acc y]
                         (if (= x y) (reduced (count acc))
                             (conj acc 1)))
                       [] xs)]
    (if (number? result)
      result
      -1)))

(defn steps-to-point
  [points point]
  (find-index point points))

(defn calc-steps
  [[wire-one wire-two]]
  (let [w1-state (calc-wire-path wire-one)
        w2-state (calc-wire-path wire-two)
        w1-points (extract-points w1-state)
        w2-points (extract-points w2-state)
        common-points (clj-set/intersection (set w1-points) (set w2-points))
        w1-steps-to-points (map #(inc (steps-to-point w1-points %)) common-points)
        w2-steps-to-points (map #(inc (steps-to-point w2-points %)) common-points)
        joined (partition 2 (interleave w1-steps-to-points w2-steps-to-points))
        steps (map #(apply + %) joined)]
    (apply min steps)))

(defn loader
  []
  (->> "resources/day3.txt"
       slurp
       string/split-lines
       (map #(string/split % #"\,"))))

(defn run
  []
  (let [data (loader)
        part-1 (calc-distance data)
        part-2 (calc-steps data)]
    (prn "3.1" part-1)
    (prn "3.2" part-2)))
