(ns day12.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.set :as st])

(defn read-file [path]
  (str/split (.trim (slurp path)) #"\n"))

(defn parse-groups [programs]
  (reduce (fn [acc, el]
            (let [links (into #{} (str/split el #" <-> |, "))
                  filt (filter #(not (empty? (st/intersection links %))) acc)]
              (if (empty? filt)
                (conj acc links)
                (conj (remove (set filt) acc) (apply st/union (conj filt links))))))
          []
          programs))

(defn -main [& args]
  (let [groups (parse-groups (read-file "input"))
        zero (count (some #(if (contains? % "0") %) groups))
        cnt (count  groups)]
    {:zero zero :groups cnt}))
