(ns day13.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.core.reducers :as r])

(defn parse-input [path]
  (partition 2 (map #(Integer/parseInt %) (str/split (slurp path) #"\n|: "))))

(defn caught? [step delay]
  (= (mod (+ delay (first step)) (* (- (second step) 1) 2)) 0))

(defn avoid-all? [steps delay]
  (not-any? #(caught? % delay) steps))

(defn solve1 [path]
  (let [layers (parse-input path)]
    (reduce #(if (caught? %2 0)
               (+ %1 (r/fold * %2))
               %1)
            0
            layers)))

(defn solve2 [path]
  (let [layers (parse-input path)]
    (first (filter some? (map #(if (avoid-all? layers %) %) (range))))))

(defn -main [& args]
  {:part1 (solve1 "input")
   :part2 (solve2 "input")})
