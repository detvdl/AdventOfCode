(ns day24.core
  (:gen-class))

(require '[clojure.string :as str])

(defn parse-input [path]
  (->> (.trim (slurp path))
       (str/split-lines)
       (into #{} (comp
                  (map #(str/split % #"/"))
                  (map (fn [x] (mapv #(Integer/parseInt %) x)))
                  ;; set = O(1) contains? = happiness
                  (map set)))))

(defn connections [port components]
  (filter #(contains? % port) components))

;; To build up all possible combinations stemming from a component,
;; we must recursively go through each of its connect-able components
;; and check their connections, etc.
;; until we are out of components to check, starting at the componenent
;; containing port 0
(defn build-bridges [components port]
  (when-let [conn (seq (connections port components))]
    (for [c conn
          ;; Working with sets *has* its downsides
          ;; in terms of terseness
          :let [next (or (first (disj c port)) port)
                rst (disj components c)]]
      (cons c (build-bridges rst next)))))

(defn tree-branches [tree]
  (if (seq? tree)
    (let [[root & children] tree]
      (if children
        (mapcat (fn [x] (map #(cons root %) (tree-branches x))) children)
        [[root]]))
    [tree]))

;; Bridges now look like '(#{0 1} (#{1 10} (#{9 10})))
;; so we need to flatten them
(defn bridge-strength [bridge]
  (let [components (map vec bridge)
        comp-str (fn [[x y]] (if y (+ x y) (* 2 x)))]
    (apply + (map comp-str components))))

(defn bridge-length [bridge]
  (count bridge))

(defn solve1 [path]
  (let [components (parse-input path)
        combinations (build-bridges components 0)
        branches (mapcat tree-branches combinations)
        bridge-strengths (map bridge-strength branches)]
    (apply max (flatten bridge-strengths))))

(defn solve2 [path]
  (let [components (parse-input path)
        combinations (build-bridges components 0)
        branches (mapcat tree-branches combinations)
        lengths-and-strengths (into [] (map (fn [b]
                                              [(count b) (bridge-strength b)])
                                            branches))]
    (first (sort-by (fn [[l s]] [(- l) (- s)]) lengths-and-strengths))))

(solve2 "input")
(prn (solve1 "input"))

(defn -main [& args]
(println "Hello, World!"))
