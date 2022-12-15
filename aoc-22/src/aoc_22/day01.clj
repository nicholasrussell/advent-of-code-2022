(ns aoc-22.day01
  (:require [clojure.string :as string]
            [aoc-22.file :as file]))

;; 1: 69912
;; 2: 208180

(defn cal-max
  [part state]
  {:max (if (= part 2)
          (take-last 3 (sort (cons (:running state) (:max state))))
          (max (:max state) (:running state)))
   :running 0})

(defn increase-run
  [state line]
  {:max (:max state)
   :running (+ (:running state) (Integer/parseInt line))})

(defn parse
  [part file]
  (:max
   (reduce
    (fn [state line]
      (if (string/blank? line)
	(cal-max part state)
	(increase-run state line)))
    {:max (if (= part 2) [] 0)
     :running 0}
    file)))

(defn -main
  [& args]
  (let [part (or (first args) 1)
        max (->> "day01/input" file/lazy-load-resource (parse part))]
    (println (if (= part 2) (reduce + 0 max) max))))

