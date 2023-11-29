(ns aoc-22.day03
  (:require [clojure.set :as sets]
            [aoc-22.file :as file]))

;; Part 1: 8072
;; Part 2: 2567

(def priorities
  (merge
   (reduce
    (fn [acc cur]
      (assoc acc (char cur) (- cur (- (int \a) 1))))
    {}
    (range (int \a) (+ (int \z) 1)))
   (reduce
    (fn [acc cur]
      (assoc acc (char cur) (+ (- cur (- (int \A) 1)) 26)))
    {}
    (range (int \A) (+ (int \Z) 1)))))

(defn compartments
  [rucksack]
  (split-at (/ (count rucksack) 2) rucksack))

(defn determine-duplicate
  [compartments]
  (->> compartments
       (map set)
       (apply sets/intersection)
       first))

(defn solve
  [rucksack]
  (->> rucksack
       compartments
       determine-duplicate
       priorities))

(defn solve2
  [rucksacks]
  (->> rucksacks
       (partition 3)
       (map determine-duplicate)
       (map priorities)))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day03/input")]
    (if (= part 2)
      (println (reduce + 0 (solve2 lines)))
      (println (reduce + 0 (map solve lines))))))
