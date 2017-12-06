(ns day6.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-file-to-array [path]
  (let [file (slurp path)
        nums (str/split file #"\s")]
    (map #(Integer/parseInt %) nums)))

(defn seen? [lst hashset]
  (some? (get hashset lst)))

(defn redistribute [lst]
  (let [maxv (apply max lst)
        pos (.indexOf lst maxv)
        t (transient lst)]
    (assoc! t pos 0)
    (dotimes [i maxv]
      (let [ind (mod (+ i pos 1) (count lst))]
        (assoc! t ind (+ (nth t ind) 1))))
    (persistent! t)))

(defn count-steps [lst]
  (loop [lst lst hs (hash-set []) cnt 0]
    (if (seen? lst hs)
      [cnt lst]
      (recur (redistribute lst) (conj hs (into [] lst)) (+ cnt 1)))))

(defn -main [& args]
  (let [arr (read-file-to-array "input")
        [steps lst] (count-steps (vec arr))
        [sec _] (count-steps lst)]
    (printf "Puzzle 1: %d\nPuzzle 2: %d\n" steps sec)))
