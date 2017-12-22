(ns day20.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.set :as st])

;; TODO: regex parse this
(defn parse-attributes [att]
  (-> (str/replace att #"<|>" "")
      (str/trim)
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

;; TODO: this too
(defn parse-input [path]
  (->> (slurp path)
       (.trim)
       (str/split-lines)
       (map #(->> (str/split % #"=|, ")
                  (partition 2)
                  (map (fn [[k v]]
                         (vec [k (parse-attributes v)])))
                  (into {})))
       (vec)))

(defn abs-sum [att]
  (apply + (map #(Math/abs %) att)))

;; all we need to do for part 1 is
;; find the minimal acceleration
;; to know which particle remains closest
;; to the origin in the long run
(defn part1 [path]
  (->> (parse-input path)
       (map-indexed #(vec [%1 (abs-sum (%2 "a"))]))
       (apply min-key second)))

;; FIXME: hash the positions to eliminate extra collision checking
;; in naive simulation version
(defn part2-simul [path]
  (let [particles (parse-input path)]
    (loop [part particles, iter 1000]
      (if (zero? iter)
        (count part)
        (let [collis (apply st/union (map-indexed #(collisions part [%1 %2]) part))
              filt (remove collis part)
              upd-veloc (map #(update-vals % (partial +) "v" "a") filt)
              upd-pos (map #(update-vals % (partial +) "p" "v") upd-veloc)]
          (recur (vec upd-pos) (dec iter)))))))

;; update value in k1 as func mapv'ed over k1 k2
(defn update-vals [m func k1 k2]
  (update-in m [k1] #(mapv func % (m k2))))

(defn collisions [coll [ind el]]
  (reduce-kv (fn [acc, k, {yp "p" :as y}]
               (if (and (not= k ind)
                        (= yp (el "p")))
                 (conj acc y el)
                 acc))
             #{}
             coll))

(defn -main [& args]
  {:puzzle1 (part1 "input") :puzzle2 (part2 "input")})
