(ns day19.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-input [path]
  (->> (slurp path)
       (str/split-lines)
       (mapv vec)))

;; keep complementary direction as last element
;; of the corresponding vector
;; this doesn't affect `mapv`
(def directions {:NORTH [-1 0 :SOUTH]
                 :EAST [0 1 :WEST]
                 :SOUTH [1 0 :NORTH]
                 :WEST [0 -1 :EAST]})

(defn change-direction [route position direction]
  (reduce (fn [acc, [k v]]
            (let [npos (mapv + position v)
                  next (get-in route npos)]
              (if (and (some? next)
                       (not (= \space next)))
                k
                acc)))
          direction
          ;; we don't want to go back the way we came
          ;; but also consider we may keep going the same
          ;; direction after a + crossroads
          (dissoc directions (last (directions direction)))))

(defn walk [route]
  (let [origin [0 (.indexOf (first route) \|)]]
    (loop [dir :SOUTH, pos origin, pipe \|, seen [], steps 0]
      ;; stop if we've gone out of bounds
      ;; or reached an empty space
      (if (or (= pipe \space)
              (not pipe))
        {:seen (apply str seen) :steps steps}
        ;; else switch directions on a + crossroads
        ;; or keep going the same direction
        (let [next-pos (mapv + pos (directions dir))
              next (get-in route next-pos)
              next-dir (case next
                         \+ (change-direction route next-pos dir)
                         dir)
              seen (if (Character/isLetter next) (conj seen next) seen)]
          (recur next-dir next-pos next seen (inc steps)))))))

(defn -main [& args]
  (walk (parse-input "input")))
