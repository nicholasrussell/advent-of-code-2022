(ns aoc-22.day04
  (:require [clojure.set :as sets]
            [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 534

(defn section-assignments
  [line]
  (->> (string/split line #",")
       (map
        (fn [assignment]
          (let [splits (string/split assignment #"-")]
            (range (Integer/parseInt (first splits)) (+ (Integer/parseInt (second splits)) 1)))))))

(defn is-full-coverage-assignment?
  [assignments]
  (let [set-assignments (map set assignments)
        union (apply sets/union set-assignments)]
    (some (partial = union) set-assignments)))

(defn solve
  [lines]
  (reduce + 0 (map #(if (is-full-coverage-assignment? (section-assignments %)) 1 0) lines)))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day04/input")]
    (if (= part 2)
      (println "TODO")
      (println (solve lines)))))
