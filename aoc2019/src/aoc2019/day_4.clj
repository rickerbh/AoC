(ns aoc2019.day-4)

(defn generate-passwords
  [start end]
  (range start (inc end)))

(defn any-true?
  [xs]
  (reduce (fn [acc x] (if x (reduced x) acc)) false xs))

(defn adjacent-same?
  [number]
  (->> number
       str
       (partition 2 1)
       (map #(apply = %))
       any-true?))

(defn is-sorted?
  [s]
  (= (sort s) (into [] s)))

(defn inc-or-same?
  [number]
  (-> number
      str
      is-sorted?))

(defn matching-passwords
  [start end]
  (->> (generate-passwords start end)
       (filter (fn [x] (and (adjacent-same? x)
                            (inc-or-same? x))))))

(defn has-unique-twos?
  [number]
  (let [pairs (->> number
                   str
                   (partition 2 1)
                   (filter #(apply = %)))
        uniques (set pairs)
        freqs (frequencies pairs)]
    (if (and (= 2 (count freqs))
             (= 2 (val (first freqs)))
             (apply = (vals freqs)))
      false
      (if (< 1 (count uniques))
        true
        (if (= (count pairs) (count uniques))
          true
          false)))))

(defn run
  []
  (let [start 197487
        end 673251
        good-pws (matching-passwords start end)
        part-1 (count good-pws)
        part-2 (count (filter has-unique-twos? good-pws))]
    (prn "4.1" part-1)
    (prn "4.2" part-2)))
