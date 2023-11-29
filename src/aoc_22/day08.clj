(ns aoc-22.day08
  (:require [clojure.pprint :as pprint]
            [aoc-22.file :as file]))

;; Part 1: 1672
;; Part 2: 327180

(defn tree-map
  [lines]
  (reduce #(concat %1 [(mapv (fn [char] (Integer/parseInt (str char))) %2)]) [] lines))

(defn transpose
  [blocks]
  (apply mapv vector blocks))

(defn visible
  [tree-map]
  (reduce
   (fn [tree-map row]
     (conj
      tree-map
      (reduce
       (fn [row tree]
         (let [m (apply max (filter some? row))]
           (conj row (if (> tree m) tree nil))))
       [(first row)]
       (rest row))))
   []
   tree-map))

(defn scenic-score
  [row]
  (if (empty? row)
    nil
    (concat (vector
             (let [tree (first row)]
               (:count (reduce
                        (fn [acc other-tree]
                          (if (not (:counting? acc))
                            acc
                            (if (< other-tree tree)
                              (assoc acc :count (inc (:count acc)))
                              (assoc acc
                                     :count (inc (:count acc))
                                     :counting? false))))
                        {:counting? true
                         :count 0}
                        (rest row)))))
            (scenic-score (rest row)))))

(defn scenic
  [tree-map]
  (reduce
   (fn [tree-map row]
     (conj
      tree-map
      (scenic-score row)))
   []
   tree-map))

(defn xform-map
  [f combine-f tree-map]
  (let [left (f tree-map)
        right (mapv #(into [] (reverse %)) (f (mapv #(into [] (reverse %)) tree-map)))
        transposed (transpose tree-map)
        down (transpose (f transposed))
        up (transpose (mapv #(into [] (reverse %)) (f (mapv #(into [] (reverse %)) transposed))))]
    (reduce
     (fn [acc cur]
       (conj acc
             (reduce
              (fn [row-acc row-cur]
                (conj row-acc
                      (combine-f row-acc row-cur)))
              []
              (transpose cur))))
     []
     (map vector left right down up))))
                      

(defn solve
  [part lines]
  (let [tree-map (tree-map lines)]
    (if (= part 2)
      (let [scenic-score-map (xform-map scenic
                                        (fn [row-acc row-cur]
                                          (reduce * 1 row-cur))
                                        tree-map)]
        (apply max (mapv #(apply max %) scenic-score-map)))
      (let [visibility-map (xform-map visible
                            (fn [row-acc row-cur]
                              (reduce
                               (fn [comb-acc comb-cur]
                                 (if (some? comb-cur)
                                   comb-cur
                                   comb-acc))
                               nil
                               row-cur))
                            tree-map)]
        (reduce + 0 (map #(count (filter some? %)) visibility-map))))))

(defn -main
  [& args]
  (let [part (first args)
        #_test-lines #_["30373"
                        "25512"
                        "65332"
                        "33549"
                        "35390"]
        lines (file/lazy-load-resource "day08/input")]
    (pprint/pprint (solve part lines))))
