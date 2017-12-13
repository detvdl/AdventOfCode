(ns day13.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.core.reducers :as r])

(defn parse-input [path]
  (partition 2 (map #(Integer/parseInt %) (str/split (slurp path) #"\n|: "))))

(defn caught? [[step range] delay]
  (zero? (mod (+ delay step) (* (- range 1) 2))))

(defn avoid-all? [steps delay]
  (not-any? #(caught? % delay) steps))

(defn solve1 [layers]
  (reduce #(if (caught? %2 0)
             (+ %1 (r/fold * %2))
             %1)
          0
          layers))

(def xf (comp (filter #(caught? % 0)) (map #(apply * %))))
(defn solve1-trans [layers]
  (transduce xf + layers))

(defn solve1-filter [layers]
  (apply + (map #(apply * %) (filter #(caught? % 0) layers))))

(defn solve2 [layers]
  (first (filter #(avoid-all? layers %) (range))))

(defn solve2-anonfn [layers]
(some (fn [delay]
        (when (not-any? (fn [step] (caught? step delay)) layers)
          delay))
      (range)))

(defn -main [& args]
  (let [layers (parse-input "input")]
    {:part1 (solve1 layers) :part2 (solve2 layers)}))
