(ns day14.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.core.reducers :as r]
         '[clojure.set :as st]
         '[clojure.pprint :refer (cl-format)])

(defn read-file [path]
  (.trim (slurp path)))

(defn lengths [input]
  (concat (map int input) '(17 31 73 47 23)))

(def numbers (into [] (range 0 256)))

(defn transform-pos [nums pos length]
  (let [numlen (count nums)]
    (if (> (+ pos length) numlen)
      (let [rem (mod (+ pos length) numlen)
            rev (->> nums
                     (repeat)
                     (apply concat)
                     (take (+ numlen rem))
                     (drop pos)
                     (reverse))]
        (concat (take-last rem rev)
                (drop rem (take pos nums))
                (drop-last rem rev)))
      (concat (take pos nums)
              (->> nums
                   (take (+ pos length))
                   (drop pos)
                   (reverse))
              (drop (+ pos length) nums)))))

(defn solve [ls init]
  (reduce
   (fn [[nums pos skip] l]
     [(transform-pos nums pos l)
      (mod (+ pos l skip) (count nums))
      (inc skip)])
   init
   ls))

(defn knot-hash [l nums]
  (let [[sparse _ _] (nth (iterate (partial solve l) [nums 0 0]) 64)
        dense (map #(apply bit-xor %) (partition 16 sparse))
        knot (apply str (map #(format "%02x" %) dense))]
    knot))

(defn knot-to-row [input num]
  (let [knot (knot-hash (lengths (str input "-" num)) numbers)]
    (apply str
           (->> knot
                (map #(Integer/parseInt (str %) 16))
                (map #(cl-format nil "~4,'0',B" %))))))

(defn part1 [input]
  (reduce #(+ %1
              (count (filter (fn [x] (= x \1))
                             (knot-to-row input %2))))
          0
          (range 128)))

(defn part1-thlast [input]
  (->> (range 128)
       (map #(knot-to-row input %))
       (map #(filter (fn [x] (= \1 x)) %))
       (map count)
       (apply +)))

;; PART 2
(def hs (into [] (map #(knot-to-row in %) (range 128))))

(defn group-ones [bin y]
  (let [parts (partition-by #(= \1 %) bin)]
    (reduce (fn [[acc, x] part]
              (let [plen (count part)
                    new-x (+ x plen)
                    xy-co (interleave (range x (+ x plen)) (repeat plen y))]
                (if (some #(= \1 %) part)
                  [(conj acc (into #{} (partition 2 xy-co))), new-x]
                  [acc, new-x])))
            [[] 0]
            parts)))

(def map-ones-to-coordinates
  (map first (mapv #(group-ones %1 %2) hs (range 128))))

(defn attached-to? [prev group]
  (let [adjacent (into #{} (map (fn [[x, y]] [x (dec y)]) group))]
    (if (not (empty? (st/intersection adjacent prev))) prev)))

(defn part2 []
  (count (reduce (fn [acc, el]
                   (let [filt (filter #(attached-to? % el) acc)]
                     (if (empty? filt)
                       (conj acc el)
                       (conj (remove (set filt) acc) (apply st/union (conj filt el))))))
                 []
                 (flatten map-ones-to-coordinates))))

(defn -main [& args]
  (let [input (read-file "input")]
    {:part1 (part1 input) :part2 (part2 input)}))
