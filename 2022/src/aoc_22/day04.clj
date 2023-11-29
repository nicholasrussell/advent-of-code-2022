(ns aoc-22.day04
  (:require [clojure.set :as sets]
            [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 534
;; Part 2: 841

(defn section-assignments
  [line]
  (->> (string/split line #",")
       (map
        (fn [assignment]
          (let [splits (string/split assignment #"-")]
            (range (Integer/parseInt (first splits)) (+ (Integer/parseInt (second splits)) 1)))))))

(defn full-coverage-assignment?
  [assignments]
  (let [set-assignments (map set assignments)
        union (apply sets/union set-assignments)]
    (some (partial = union) set-assignments)))

(defn overlapping-assignment?
  [assignments]
  (> (count (apply sets/intersection (map set assignments))) 0))

(defn solve
  [lines]
  (reduce + 0 (map #(if (full-coverage-assignment? (section-assignments %)) 1 0) lines)))

(defn solve2
  [lines]
  (reduce + 0 (map #(if (overlapping-assignment? (section-assignments %)) 1 0) lines)))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day04/input")]
    (if (= part 2)
      (println (solve2 lines))
      (println (solve lines)))))
