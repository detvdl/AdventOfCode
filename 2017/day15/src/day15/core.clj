(ns day15.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.core.reducers :as r])

(defn gen-next [x mult]
  (-> x
      (* mult)
      (rem 2147483647)))

(defn generator [init mult pred]
  (->> (iterate #(gen-next % mult) init)
       rest ;; skip initial element
       (filter pred)))

(defn match-l16? [a b]
  (let [mask (fn [x] (bit-and 0xffff x))]
    (= (mask a) (mask b))))

(defn count-all [f coll]
  (reduce (fn [acc, el]
            (if (f el)
              (inc acc)
              acc))
          0
          coll))

(defn solve [input mults npairs preds]
  (->> (mapv generator input mults preds)
       (apply map vector) ;; outputs pairs of [a, b] generator values
       (take npairs)
       (count-all #(apply match-l16? %))))

(def input [873 583])
(def mults [16807 48271])
(def part1 (solve input mults 40000000 [identity identity]))
(def part2 (solve input mults 5000000 [#(zero? (rem % 4))
                                       #(zero? (rem % 8))]))

(defn -main [& args]
  {:puzzle1 part1 :puzzle2 part2})
