(ns aoc-22.day09
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 6190
;; Part 2: 2516

(defn move
  [knot dir]
  (let [change (condp = dir
                 "U" [[1] inc]
                 "R" [[0] inc]
                 "D" [[1] dec]
                 "L" [[0] dec]
                 [[0] identity])]
    (update-in knot (first change) (second change))))

(defn pos-diff
  [knot1 knot2]
  [(- (first knot1) (first knot2)) (- (second knot1) (second knot2))])

(defn touching?
  ([knot1 knot2]
   (touching? (pos-diff knot1 knot2)))
  ([diff]
   (every? #(<= -1 % 1) diff)))

(defn catch-up
  [knot1 knot2]
  (let [diff (pos-diff knot1 knot2)]
    (if (touching? diff)
      knot2
      (let [x-diff (first diff)
            y-diff (second diff)
            x-delta (if (pos? x-diff) "R" (if (neg? x-diff) "L" nil))
            y-delta (if (pos? y-diff) "U" (if (neg? y-diff) "D" nil))]
        (-> knot2
            (move x-delta)
            (move y-delta))))))

(defn parse-move-line
  [line]
  (let [splits (string/split line #" ")]
    {:direction (first splits)
     :number (Integer/parseInt (second splits))}))

(defn mark
  [board pos]
  (conj board pos))

(defn solve
  [part lines]
  (let [state (reduce
                (fn [acc line]
                  (let [move-line (parse-move-line line)]
                    (reduce
                     (fn [state n]
                       (let [knots (reduce
                                    (fn [knots knot-idx]
                                      (let [new-pos (catch-up (nth knots (dec knot-idx)) (nth knots knot-idx))]
                                        (update-in knots [knot-idx] (constantly new-pos))))
                                    (update-in (:knots state) [0] (fn [_] (move (first (:knots state)) (:direction move-line))))
                                    (range 1 (count (:knots state))))]
                         {:board (mark (:board state) (last knots))
                          :knots knots}))
                     acc
                     (range (:number move-line)))))
                {:board (mark (set []) [0 0])
                 :knots (mapv (fn [_] (vector 0 0)) (range (if (= part 2) 10 2)))}
                lines)]
    (count (:board state))))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day09/input")]
    (pprint/pprint (solve part lines))))
