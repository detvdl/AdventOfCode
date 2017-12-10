(ns day8-clj.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-file [path]
  (str/split (slurp path) #"\n"))

(defn operation [expr ht]
  (let [[a op b] (str/split expr #"\s")
        a-val (get ht a 0)]
    (cond
      (= op "dec") (- a-val (Integer/parseInt b))
      (= op "inc") (+ a-val (Integer/parseInt b))
      :else a-val)))

(defn resolve-or-convert [comparison]
  (if (= comparison "!=")
    (partial (comp not =))
    (partial (resolve (symbol comparison)))))

(defn condition [expr ht]
  (let [[x comp y] (str/split expr #"\s+")
        x-val (get ht x 0)]
    ((resolve-or-convert comp) x-val (Integer/parseInt y))))

(defn process-input [expressions]
  (reduce #(let [expr (str/split %2 #" if ")]
             (if (condition (second expr) %1)
               (let [reg (first (str/split (first expr) #" "))
                     val (operation (first expr) %1)
                     ht (assoc-in %1 [reg] val)]
                 (update-in ht [:max] (fnil (fn [x](max val x)) 0)))
               %1))
          {}
          expressions))

(defn -main [& args]
  (let [result (process-input (read-file "input"))]
    (take 2 (sort-by val > result))))
