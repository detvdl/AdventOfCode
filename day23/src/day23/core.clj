(ns day23.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-instruction [inst]
  (let [[op a b] (str/split inst #"\s+")]
    (case op
      "set" [:modify a b (fn [_ b] b)]
      "sub" [:modify a b (partial -)]
      "mul" [:modify a b (partial *)]
      "jnz" [:jump a b])))

(defn is-register? [r]
  (contains? (into #{} (range 97 123)) (int r)))

(defn reg-value [registers r]
  (if (is-register? (first r))
    (get registers r 0)
    (Integer/parseInt r)))

(defn modify-reg [registers reg val func]
  (let [val (reg-value registers val)]
    (assoc registers reg (func (get registers reg 0) val)
           :eip (inc (registers :eip)))))

(defn jump [registers reg val]
  (let [val (reg-value registers val)
        r (reg-value registers reg)]
    (if (zero? r)
      (update registers :eip inc)
      (update registers :eip (partial + val)))))

(defn execute-instr [[tag & args] reg]
  (apply ({:modify modify-reg
           :jump jump} tag)
         reg
         args))

(defn parse-input [path]
  (->> (.trim (slurp path))
       (str/split-lines)
       (mapv parse-instruction)))

(defn solve1 [path]
  (let [instructions (parse-input path)]
    (loop [reg {:eip 0},  mul 0]
      (if (not (get instructions (reg :eip)))
        mul
        (let [instr (instructions (reg :eip))
              n-reg (execute-instr instr reg)
              n-mul (if (= (last instr) (partial *))
                      (inc mul)
                      mul)]
          (recur n-reg n-mul))))))

;; Part 2

;; you eventually want `sub b c` to be 0 so instruction 30 ends the program by jumping off of the instruction set.
;; In my case, this means b must be 124_900.
;; However, due to b increasing, instruction 16 will sometimes not execute, causing 25 to skip over incrementing `h`.
;; The amount of times we do *NOT* skip over incrementing `h`, is equal to the amount of non-prime numbers between
;; 107_900 and 124_900, because primes will cause the program to skip setting `f` to `0` and will skip over incrementing h. Alternatively, we could check the amount of times it does skip, and substract this from 10_000.

(defn prime? [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %))
                 (range 2 (Math/sqrt n)))))

(defn solve2 []
  (count (filter #(not (prime? %)) (range 107900 124917 17))))

(defn -main [& args]
  {:puzzle1 (solve1 "input") :puzzle2 (solve2)})
