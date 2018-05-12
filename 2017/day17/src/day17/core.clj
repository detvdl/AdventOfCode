(ns day17.core
  (:gen-class))

(defn spin [[buffer pos] rotations]
  (let [idx (inc (rem (+ pos rotations) (count buffer)))
        m (apply max buffer)
        [fst lst] (split-at idx buffer)]
    [(vec (concat fst [(inc m)] lst)) idx]))

(defn part1 [init rot]
  (let [[lst idx] (nth (iterate #(spin % rot) init) 2017)]
    (nth lst (inc idx))))

;; nice try, but I don't have 200 minutes the time
(defn bad-part2 [init rot]
  (let [[lst _] (nth (iterate #(spin % rot) init) 50000000)]
    (second lst)))

;; Only keep a reference to the current position,
;; buffer size (== current value) and second element
;; to avoid having to build a 50 million element buffer
(defn part2 [pos val rot]
  (loop [pos pos, val val, snd 0]
    (if (= (+ val 1) 50000000)
      snd
      (let [pos (inc (rem (+ pos rot) val))
            snd (if (= 1 pos) val snd)]
        (recur pos (inc val) snd)))))

(defn -main [& args]
  {:puzzle1 (part1 [[0] 0] 363) :puzzle2 (part2 0 1 363)})
