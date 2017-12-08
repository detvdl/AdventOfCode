(ns day6.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-file [path]
  (mapv #(Integer/parseInt %)
        (str/split (slurp path) #"\s+")))

;; Version 1: using transient vectors
;; for internal mutability
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
    (if (contains? hs lst)
      [cnt lst]
      (recur (redistribute lst) (conj hs (into [] lst)) (+ cnt 1)))))

;; Version 2: create new lists (immutable)
(defn redistribute-imut [lst]
  (let [maxm (apply max lst)
        ind (.indexOf lst maxm)]
    (->> (concat (repeat ind 0) [(- maxm)] (repeat maxm 1))
         (partition (count lst) (count lst) (repeat 0))
         (apply mapv + lst))))

(defn count-steps-imut [lst]
  (loop [lst lst, seen {lst 0}, cnt 1]
    (let [lst* (redistribute-imut lst)]
      (if (seen lst*)
        {:puzzle1 cnt :puzzle2 (- (+ cnt 1) (seen lst*))}
        (recur lst* (assoc seen lst cnt) (inc cnt))))))

(defn -main [& args]
  (let [arr (parse-file "input")
        [steps lst] (count-steps (vec arr))
        [sec _] (count-steps lst)
        v2 (count-steps-imut arr)]
    (printf "Puzzle 1v1: %d\nPuzzle 2v1: %d\n" steps sec)
    (printf "Puzzle 1v2: %d\nPuzzle 2v2: %d" (v2 :puzzle1) (v2 :puzzle2))))
