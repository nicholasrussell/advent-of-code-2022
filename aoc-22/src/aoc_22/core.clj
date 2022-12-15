(ns aoc-22.core
  (:require [aoc-22.day01]
            [aoc-22.day02]
            [aoc-22.day03]))

(defn- exec-day
  [day part]
  (let [ns-name (str "aoc-22.day" (when (< day 10) "0") day)
        main (resolve (symbol ns-name "-main"))]
    (@main part)))

(defn -main
  [& args]
  (if (empty? args)
    (println "aoc-22 {day#} {part#}")
    (exec-day (Integer/parseInt (first args)) (Integer/parseInt (or (second args) "1")))))
