(ns aoc-22.day10
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 15360
;; Part 2: PHLHJGZA

(defn parse-instruction
  [instruction]
  (let [splits (string/split instruction #" ")]
    {:instruction (first splits)
     :args (if (> (count splits) 1) (rest splits) [])}))

(defn noop
  []
  [identity])

(defn addx
  [args]
  (let [v (Integer/parseInt (first args))]
    [identity (fn [state] (update-in state [:x] (fn [old v] (+ old v)) v))]))

(defn plan-instruction
  [cycles instruction]
  (let [fn (condp = (:instruction instruction)
             "addx" #(addx (:args instruction))
             noop)]
    (concat cycles (fn))))

(defn eval-cycle
  [cycles n]
  (update-in cycles [n] (fn [cycle prev-cycle-state] (cycle prev-cycle-state)) (nth cycles (- n 1))))

(defn draw
  [state n]
  (if (<= (- (:x state) 1) n (+ (:x state) 1))
    "#"
    "."))
 
(defn solve
  [part lines]
  (let [plan (into
              []
              (reduce
               (fn [plan line]
                 (plan-instruction plan (parse-instruction line)))
               []
               lines))
        initial-state {:x 1}
        evald (concat [initial-state] (reduce eval-cycle (update-in plan [0] (fn [cycle initial-state] (cycle initial-state)) initial-state) (range 1 (count plan))))]
    (if (= part 2)
      (let [pixels (map-indexed (fn [idx state] (draw state (mod idx 40))) evald)]
        (doall (map println (map string/join (partition 40 pixels))))
        'done)
      (reduce + 0 (map #(* (:x (nth evald (- % 1))) %) [20 60 100 140 180 220])))))

(defn -main
  [& args]
  (let [part (first args)
        test-lines ["addx 15"
                    "addx -11"
                    "addx 6"
                    "addx -3"
                    "addx 5"
                    "addx -1"
                    "addx -8"
                    "addx 13"
                    "addx 4"
                    "noop"
                    "addx -1"
                    "addx 5"
                    "addx -1"
                    "addx 5"
                    "addx -1"
                    "addx 5"
                    "addx -1"
                    "addx 5"
                    "addx -1"
                    "addx -35"
                    "addx 1"
                    "addx 24"
                    "addx -19"
                    "addx 1"
                    "addx 16"
                    "addx -11"
                    "noop"
                    "noop"
                    "addx 21"
                    "addx -15"
                    "noop"
                    "noop"
                    "addx -3"
                    "addx 9"
                    "addx 1"
                    "addx -3"
                    "addx 8"
                    "addx 1"
                    "addx 5"
                    "noop"
                    "noop"
                    "noop"
                    "noop"
                    "noop"
                    "addx -36"
                    "noop"
                    "addx 1"
                    "addx 7"
                    "noop"
                    "noop"
                    "noop"
                    "addx 2"
                    "addx 6"
                    "noop"
                    "noop"
                    "noop"
                    "noop"
                    "noop"
                    "addx 1"
                    "noop"
                    "noop"
                    "addx 7"
                    "addx 1"
                    "noop"
                    "addx -13"
                    "addx 13"
                    "addx 7"
                    "noop"
                    "addx 1"
                    "addx -33"
                    "noop"
                    "noop"
                    "noop"
                    "addx 2"
                    "noop"
                    "noop"
                    "noop"
                    "addx 8"
                    "noop"
                    "addx -1"
                    "addx 2"
                    "addx 1"
                    "noop"
                    "addx 17"
                    "addx -9"
                    "addx 1"
                    "addx 1"
                    "addx -3"
                    "addx 11"
                    "noop"
                    "noop"
                    "addx 1"
                    "noop"
                    "addx 1"
                    "noop"
                    "noop"
                    "addx -13"
                    "addx -19"
                    "addx 1"
                    "addx 3"
                    "addx 26"
                    "addx -30"
                    "addx 12"
                    "addx -1"
                    "addx 3"
                    "addx 1"
                    "noop"
                    "noop"
                    "noop"
                    "addx -9"
                    "addx 18"
                    "addx 1"
                    "addx 2"
                    "noop"
                    "noop"
                    "addx 9"
                    "noop"
                    "noop"
                    "noop"
                    "addx -1"
                    "addx 2"
                    "addx -37"
                    "addx 1"
                    "addx 3"
                    "noop"
                    "addx 15"
                    "addx -21"
                    "addx 22"
                    "addx -6"
                    "addx 1"
                    "noop"
                    "addx 2"
                    "addx 1"
                    "noop"
                    "addx -10"
                    "noop"
                    "noop"
                    "addx 20"
                    "addx 1"
                    "addx 2"
                    "addx 2"
                    "addx -6"
                    "addx -11"
                    "noop"
                    "noop"
                    "noop"]
        lines (file/lazy-load-resource "day10/input")]
    (pprint/pprint (solve part lines))))
