(ns aoc-22.day05
  (:require [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: VGBBJCRMN
;; Part 2: LBBVJBRMH

(defn parse-blocks
  [line]
  (->> line
       (partition-all 4)
       (map #(remove #{\space \[ \]} %))
       (map first)
       (into [])))

(defn transpose
  [blocks]
  (apply mapv vector blocks))

(defn finalize-block-state
  [blocks]
  (->> blocks
       butlast ; remove the column numbers
       transpose
       (mapv #(filterv some? %))
       (mapv reverse)
       (mapv #(into [] %))))

(defn move-block
  [blocks from to]
  (let [block (peek (nth blocks from))
        from-col (pop (nth blocks from))
        to-col (conj (nth blocks to) block)]
    (assoc blocks from from-col to to-col)))

(defn move-block-stack
  [blocks num-blocks from to]
  (let [block-stack (into [] (take-last num-blocks (nth blocks from)))
        from-col (into [] (drop-last num-blocks (nth blocks from)))
        to-col (into [] (concat (into [] (nth blocks to)) block-stack))]
    (assoc blocks from from-col to to-col)))

(defn move-blocks
  [blocks move crane-type]
  (if (= crane-type :9001)
    (move-block-stack blocks (:num-blocks move) (:from move) (:to move))
    (reduce
     (fn [blocks move]
       (move-block blocks (:from move) (:to move)))
     blocks
     (repeatedly (:num-blocks move) (constantly move)))))

(defn parse-move
  [line]
  (let [matches (re-matches #"move (\d+) from (\d+) to (\d+)" line)]
    {:num-blocks (Integer/parseInt (nth matches 1))
     :from (- (Integer/parseInt (nth matches 2)) 1)
     :to (- (Integer/parseInt (nth matches 3)) 1)}))

(defn parse-file
  [lines crane-type]
  (reduce
   (fn [state line]
     (if (not (:blocks-parsed? state))
       (if (string/blank? line) ; Ready to start parsing moves
         (assoc state
                :blocks-parsed? true
                :blocks (finalize-block-state (:blocks state)))
         (let [parsed (parse-blocks line)]
           (assoc state :blocks (conj (:blocks state) parsed))))
       (let [move (parse-move line)]
         (assoc state :blocks (move-blocks (:blocks state) move crane-type)))))
   {:blocks-parsed? false
    :blocks []}
   lines))

(defn get-top-blocks
  [blocks]
  (string/join (map peek blocks)))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day05/input")]
    (println (get-top-blocks (:blocks (parse-file lines (if (= part 2) :9001 :9000)))))))
