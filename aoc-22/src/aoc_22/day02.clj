(ns aoc-22.day02
  (:require [clojure.string :as string]
            [aoc-22.file :as file]))

;; Part 1: 9241
;; Part 2: 14610

;; A/X: Rock
;; B/Y: Paper
;; C/Z: Scissors

(defn play-char->play-symbol
  [char]
  (cond
    (or (= char \A) (= char \X)) :rock
    (or (= char \B) (= char \Y)) :paper
    (or (= char \C) (= char \Z)) :scissors))

(defn play-char->play-result-symbol
  [char]
  (condp = char
    \X :lose
    \Y :draw
    \Z :win
    (play-char->play-symbol char)))

(defn play->play-char
  [input]
  (char (first input)))

(defn line->plays
  [line have-play?]
  (map (comp (if have-play? play-char->play-symbol play-char->play-result-symbol) play->play-char) (string/split line #" ")))

(defn play-score
  [play]
  ; (- (int play) (int \W))
  (condp = play
    :rock 1
    :paper 2
    :scissors 3))

(defn result-score
  [result]
  (condp = result
    :win 6
    :lose 0
    :draw 3))

(def beats {:rock :scissors :scissors :paper :paper :rock})
(def win-lose (map (fn [play] {:win play :lose (beats play)}) [:rock :paper :scissors]))

(defn determine-my-play
  [opponent-play desired-result]
  (condp = desired-result
    :draw opponent-play
    :lose (beats opponent-play)
    :win (:win (first (filter #(= (:lose %) opponent-play) win-lose)))))

(defn determine-result
  [opponent-play my-play]
  (if (= opponent-play my-play)
    :draw
    (if (= my-play (beats opponent-play))
      :lose
      :win)))

(defn score-round
  [[opponent-play mine] have-play?]
  (let [my-play (if have-play? mine (determine-my-play opponent-play mine))]
    (+ (play-score my-play) (result-score (determine-result opponent-play my-play)))))

(defn -main
  [& args]
  (let [part (first args)
        lines (file/lazy-load-resource "day02/input")]
    (if (= part 2)
      (println (reduce + 0 (map (fn [round] (score-round (line->plays round false) false)) lines)))
      (println (reduce + 0 (map (fn [round] (score-round (line->plays round true) true)) lines))))))

