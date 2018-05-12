(ns day22.core
  (:gen-class))

(require '[clojure.string :as str])

;; infinite grids are easily representable by
;; a map of coordinates, assoc'ing new coordinates
;; as you go.
(defn parse-input [path]
  (->> (.trim (slurp path))
       (str/split-lines)
       (map-indexed (fn [i row]
                      (map-indexed
                       (fn [j col]
                         [[i j] col])
                       row)))
       (apply concat)
       (into {})))

(def state-transitions1 {\. \#
                         \# \.})

(defn change-state [{[y x :as loc] :loc,
                     [dy dx]       :dir
                     inf           :infected
                     :as nodes}]
  (let [state (get nodes loc \.)
        ndir (case state
               \. [(- dx) dy]
               \# [dx (- dy)])
        nstate (state-transitions1 state)
        infected (if (= \# nstate) inc identity)]
    (assoc nodes
           loc nstate
           :dir ndir
           :loc (mapv + loc ndir)
           :infected (infected inf))))

(defn solve1 [path iterations]
  (let [nodes (parse-input path)
        mid (quot (count (keys nodes)) 2)
        origin (nth (sort-by (fn [[y x]] [(- y) (- x)]) (keys nodes)) mid)
        direction [-1 0]]
    (loop [coll (merge nodes {:infected 0, :dir direction, :loc origin}), it 0]
      (if (= iterations it)
        (coll :infected)
        (let [ncoll (change-state coll)]
          (recur ncoll (inc it)))))))

(def state-transitions2 {\. \W
                         \W \#
                         \# \F
                         \F \.})

(defn change-state2 [{[y x :as loc] :loc,
                      [dy dx]       :dir
                      inf           :infected
                      :as nodes}]
  (let [state (get nodes loc \.)
        ndir (case state
               \. [(- dx) dy]
               \# [dx (- dy)]
               \W [dy dx]
               \F [(- dy) (- dx)])
        nstate (state-transitions2 state)
        infected (if (= \# nstate) inc identity)]
    (assoc nodes
           loc nstate
           :dir ndir
           :loc (mapv + loc ndir)
           :infected (infected inf))))

(defn solve2 [path iterations]
  (let [nodes (parse-input path)
        mid (quot (count (keys nodes)) 2)
        origin (nth (sort-by (fn [[y x]] [(- y) (- x)]) (keys nodes)) mid)
        direction [-1 0]]
    (loop [coll (merge nodes {:infected 0, :dir direction, :loc origin}), it 0]
      (if (= iterations it)
        (coll :infected)
        (let [ncoll (change-state2 coll)]
          (recur ncoll (inc it)))))))

(defn -main [& args]
  {:puzzle1 (solve1 "input" 10000) :puzzle2 (solve2 "input" 10000000)})
