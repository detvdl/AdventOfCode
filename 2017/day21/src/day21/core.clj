(ns day21.core
  (:gen-class))

(require '[clojure.string :as str])

;; rotating a pattern with `transpose` creates only 2 rotations:
;; the first transposition and the original (since (M^T)^T == M)
;; so we reverse (flip) each transposition to create all 4 rotations
(def transpose (partial apply map vector))
(def rotate (comp reverse transpose))

(defn flips-and-rotations [pattern]
  (let [rotations (take 4 (iterate rotate pattern))]
    (concat rotations (map reverse rotations))))

(defn parse-input [path]
  (->> (.trim (slurp path))
       (str/split-lines)
       (map #(str/split % #" => "))
       (map (fn [rule]
              (let [[pat trans] (map #(str/split % #"\/") rule)]
                (zipmap (flips-and-rotations (map seq pat))
                        (repeat (map seq trans))))))
       (reduce merge {})))

" we need to transform a pattern of the form
((\# \. \. \.)
 (\. \# \. \.)
 (\. \. \# \.)
 (\. \. \. \#))

into equally sized square patterns like so:

1. ((\# \.)
    (\. \#))

2. ((\. \.)
    (\. \.))

3. ((\. \.)
    (\. \.))

4. ((\# \.)
    (\. \#))

So we partition each row into 2/3
Then we partition into rows themselves
"
(defn partition-grid [grid size]
  (->> (map #(partition size %) grid)
       (partition-all size)
       (map transpose)))

(defn join-grid [grid]
  (->> (mapcat transpose grid)
       (map flatten)))

(defn apply-pattern [grid patterns]
  (let [part-size (if (even? (count (first grid))) 2 3)
        parts (partition-grid grid part-size)
        new (map #(map patterns %) parts)]
    (join-grid new)))

(defn solve [path it]
  (let [patterns (parse-input path)
        start '((\. \# \.)
                (\. \. \#)
                (\# \# \#))
        end (nth (iterate #(apply-pattern % patterns) start) it)]
    (count (filter #{\#} (flatten end)))))

(defn -main [& args]
  :puzzle1 (solve "input" 5) :puzzle2 (solve "input" 18))
