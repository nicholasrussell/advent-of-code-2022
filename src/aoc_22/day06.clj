(ns aoc-22.day06
  (:require [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 1538
;; Part 2: 2315

(defn update-run
  [run char marker-len]
  (into [] (concat (if (>= (count run) marker-len) (rest run) run) [char])))

(defn marker?
  [run marker-len]
  (and (>= (count run) marker-len) (= (count (set run)) (count run))))

(defn solve
  [line marker-len]
  (reduce
   (fn [state char]
     (let [updated-run (update-run (:run state) char marker-len)
           updated-position (+ 1 (:position state))]
       (cond
         (marker? updated-run marker-len)
         (reduced updated-position)

         :else
         {:run updated-run
          :position updated-position})))
   {:run []
    :position 0}
   line))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day06/input")]
    (println (solve (first lines) (if (= part 2) 14 4)))))

