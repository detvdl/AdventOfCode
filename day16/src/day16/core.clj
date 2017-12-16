(ns day16.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-input [path]
  (-> (slurp path)
      (str/split #",")))

(defn spin [size coll]
  (vec (concat (take-last size coll) (drop-last size coll))))

(defn swap-pos [p1 p2 coll]
  (assoc coll p2 (coll p1) p1 (coll p2)))

(defn swap-prog [p1 p2 coll]
  (let [[p1 p2] (map #(.indexOf coll (first %)) [p1 p2])]
    (swap-pos p1 p2 coll)))

(defn parse-move [[m & ms] coll]
  (let [[a b] (str/split (apply str ms) #"/")]
    (cond
      (= m \s) (spin (Integer/parseInt a) coll)
      (= m \x) (swap-pos (Integer/parseInt a) (Integer/parseInt b) coll)
      (= m \p) (swap-prog a b coll))))

(defn part1 [input start]
  (reduce #(parse-move %2 %1)
          start
          input))

(defn part2 [input start]
  (loop [in input, st start, seen []]
    (if (some #(= st %) seen)
      (let [idx (rem 1000000000 (count seen))]
        (nth seen idx))
      (let [new-st (part1 input st)]
        (recur in new-st (conj seen st))))))

(->> (part1 (parse-input "input") (vec "abcdefghijklmnop"))
     (apply str)
     (prn))

(->> (part2 (parse-input "input") (vec "abcdefghijklmnop"))
     (apply str)
     (prn))

(defn -main [& args])
