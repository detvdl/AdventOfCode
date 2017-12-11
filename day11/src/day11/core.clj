(ns day11.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-file [path]
  (str/split (slurp path) #","))

(def directions {"n" {:y 1} "s" {:y -1}
                 "ne" {:x 1 :y 1} "se" {:x 1 :y -1}
                 "sw" {:x -1 :y -1} "nw" {:x -1 :y 1}})

(defn merge-coords [dir coords]
  (let [vec (get directions dir)]
    (merge-with + coords vec)))

(defn vec-abs [vec]
  (map #(Math/abs %) vec))

(defn -main [& args]
  (loop [dirs (read-file "input")
         origin {:x 0 :y 0}
         m 0]
    (if (empty? dirs)
      {:dist (apply max (vec-abs (vals origin))) :max m}
      (let [new-origin (merge-coords (first dirs) origin)
            m (apply max m (vec-abs (vals new-origin)))]
        (recur (rest dirs) new-origin m)))))
