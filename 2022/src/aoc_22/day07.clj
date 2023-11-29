(ns aoc-22.day07
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [aoc-22.file :as file]))

;; Part 1: 1513699
;; Part 2: 7991939

(defn make-directory
  [name]
  {:name name
   :type :directory})

(defn make-file
  [name size]
  {:name name
   :type :file
   :size size})

(defn directory?
  [file]
  (= (:type file) :directory))

(defn file?
  [file]
  (= (:type file) :file))

(defn make-node
  [path file]
  {:path path
   :file file
   :children nil})

(defn initialize-fs
  []
  (make-node ["/"] (make-directory "/")))

(defn insert
  [fs new-node]
  (defn should-insert?
    [node new-node]
    (and (directory? (:file node))
         (or (and (and (= (:path node) ["/"]) (= (:name (:file node)) "/"))
                  (= (:path new-node) ["/"]))
             (= (into [] (concat (:path node) [(:name (:file node))])) (:path new-node)))
         (not-any? #(= (:name (:file %)) (:name (:file new-node))) (:children node))))
  (defn insert-helper
    [node new-node]
    (cond
      (nil? node) nil
      (should-insert? node new-node) (assoc node :children (into [] (concat (:children node) [new-node])))
      :else (assoc node :children (mapv #(insert-helper % new-node) (:children node)))))
  (if (and (= (:path new-node) ["/"])
           (= (:name (:file new-node)) "/"))
    fs
    (insert-helper fs new-node)))

(defn command?
  [line]
  (string/starts-with? line "$"))

(defn parse-command
  [raw-command]
  (let [command-line (filter some? (re-matches #"\$ ([^ ]+) ?(.+)*" (first raw-command)))]
    {:command (nth command-line 1)
     :args (if (> (count command-line) 2) (into [] (string/split (nth command-line 2) #" ")) [])
     :output (into [] (rest raw-command))}))

(defn cd
  [context command]
  (let [arg (first (:args command)) ; right now, assume only changing one directory at a time and no additional args
        new-path (condp = arg
                   "/" ["/"]
                   ".." (let [new (pop (:current-path context))]
                          (if (empty? new)
                            ["/"]
                            new))
                   (conj (:current-path context) arg))
        new-fs (if (= new-path ["/"])
                 (:fs context)
                 (insert (:fs context) (make-node (into [] (butlast new-path)) (make-directory (peek new-path)))))]
    (assoc context :fs new-fs :current-path new-path)))

(defn ls
  [context command]
  (reduce
   (fn [context output]
     (let [splits (string/split output #" ")
           dir? (= (first splits) "dir")
           node (make-node (:current-path context) (if dir? (make-directory (second splits)) (make-file (second splits) (Integer/parseInt (first splits)))))]
      (assoc context :fs (insert (:fs context) node))))
   context
   (:output command)))

(defn dir-sizes
  [node]
  (if (file? (:file node))
    node
    (let [children (mapv dir-sizes (:children node))]
      (assoc (update-in node [:file] #(assoc % :size (reduce + 0 (map (fn [child] (:size (:file child))) children)))) :children children))))

(defn sum-dirs-size-at-most-n
  [fs n]
  (let [fs-with-dir-sizes (dir-sizes fs)]
    (letfn [(helper [node]
              (+ (reduce + 0 (map helper (:children node)))
                 (if (and (directory? (:file node))
                          (< (:size (:file node)) n))
                   (:size (:file node))
                   0)))]
      (helper fs-with-dir-sizes))))

(defn free-up-space
  [fs total-space desired-space]
  (let [fs (dir-sizes fs)
        used-space (:size (:file fs))
        free-space (- total-space used-space)
        min-to-free (- desired-space free-space)]
    (letfn [(flat-helper [node]
              (if (file? (:file node))
                nil
                (into [] (concat [(dissoc node :children)] (into [] (flatten (remove nil? (mapv flat-helper (:children node)))))))))]
      (let [flat (flat-helper fs)
            to-delete (reduce
                       (fn [current node]
                         (if (and (>= (:size (:file node)) min-to-free) (< (:size (:file node)) (:size (:file current))))
                           node
                           current))
                       {:file {:size total-space}}
                       flat)]
        (:size (:file to-delete))))))

(defn solve
  [part fs raw-commands]
  (let [fs-state (reduce
                  (fn [state raw-command]
                    (let [command (parse-command raw-command)]
                      ((condp = (:command command)
                         "cd" cd
                         "ls" ls
                         (fn [state command] state))
                       state command)))
                  {:fs fs
                   :current-path ["/"]}
                  raw-commands)]
    (if (= part 2)
      (free-up-space (:fs fs-state) 70000000 30000000)
      (sum-dirs-size-at-most-n (:fs fs-state) 100000))))

(defn day7-reader
  [reader]
  (defn read-command
    [reader command output]
    (if-let [line (.readLine reader)]
      (if (command? line)
        (lazy-seq (cons (into [] (concat [command] output)) (read-command reader line [])))
        (read-command reader command (concat output [line])))
      (lazy-seq (do
                  (.close reader)
                  (cons (into [] (concat [command] output)) nil)))))
  (if-let [command (.readLine reader)]
    (read-command reader command [])
    (do
      (.close reader)
      nil)))

(defn -main
  [& args]
  (let [part (first args)
        fs (initialize-fs)
        raw-commands (file/lazy-load-resource "day07/input" day7-reader)]
    (pprint/pprint (solve part fs raw-commands))))
    
