(ns day25.core
  (:gen-class))

(defn change-state [machines tape]
  (let [{state :state, idx :eip, slots :slots} tape
        val (get slots idx 0)
        [write cursor nstate] (get-in machines [state val])]
    (assoc tape
           :state nstate
           :eip (+ idx cursor)
           :slots (assoc slots idx write))))

;; {:state {:value [write move new-state]}}
(def state-machines
  {"A" {0 [1 1 "B"],  1 [0 -1 "C"]}
   "B" {0 [1 -1 "A"], 1 [1 1 "D"]}
   "C" {0 [0 -1 "B"], 1 [0 -1 "E"]}
   "D" {0 [1 1 "A"],  1 [0 1 "B"]}
   "E" {0 [1 -1 "F"], 1 [1 -1 "C"]}
   "F" {0 [1 1 "D"],  1 [1 1 "A"]}
   })

(defn solve1 []
  (let [tape {:state "A"
              :eip 0
              :slots {0 0}}
        end-tape (nth (iterate (partial change-state state-machines) tape) 12667664)]
    (count (filter #(= 1 %) (vals (end-tape :slots))))))

(defn -main [& args]
  {:puzzle1 (solve1)})
