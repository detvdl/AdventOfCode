(ns day18.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.core.async :as async])

(defn parse-input [path]
  (str/split-lines (.trim (slurp path))))

(defn number-or-keyword [arg]
  (let [rd (read-string arg)]
    (if (number? rd) rd (keyword arg))))

(defn parse-instructions [instrs]
  (mapv #(let [[instr & args] (str/split % #"\s+")
               args (mapv number-or-keyword args)]
           (case instr
             "add" (into [:MODIFY] (into args [(partial +)]))
             "mul" (into [:MODIFY] (into args [(partial *)]))
             "mod" (into [:MODIFY] (into args [(partial mod)]))
             "set" (into [:MODIFY] (into args [(partial (fn [_ b] b))]))
             "snd" (into [:SEND] args)
             "rcv" (into [:RECOVER] args)
             "jgz" (into [:JUMP] args)))
        instrs))

(defn modify [registers a b func & rest]
  (async/go
    (let [select (fn [x] (if (number? x) x (get registers x 0)))
          [a-val b-val] (map select [a b])
          eip (get registers :eip 0)]
      (assoc registers
             a (func a-val b-val)
             :eip (inc eip)))))

(defn send-freq [registers freq & channels]
  (async/go
    (let [freq-val (if (number? freq) freq (get registers freq 0))
          eip (get registers :eip 0)]
      (if (some? channels)
        (let [[in out] channels]
          (if (= (registers :id) 1)
            (prn (str "Program 1: sending packet nr." (get registers :sent 1) "...")))
          (async/put! out freq-val)))
      (assoc registers
             :mrp freq-val
             :eip (inc eip)
             :sent (inc (get registers :sent 0))))))

(defn recover-freq [registers return & channels]
  (async/go
    (let [return-val (if (number? return) return (get registers return 0))
          eip (get registers :eip 0)]
      (if (some? channels)
        (let [[in out] channels
              freq (async/<! in)]
          (do (prn (str "receiving " freq))
              (assoc registers return freq :eip (inc eip))))
        (if (zero? return-val)
          (assoc registers :eip (inc eip))
          (assoc registers
                 :rec (registers :mrp)
                 :eip (inc eip)))))))

;; we keep an instruction pointer register
;; so we don't have to keep passing around the current index
(defn jump [registers reg jmp & rest]
  (async/go
    (let [select (fn [x] (if (number? x) x (get registers x 0)))
          [val jmp] (mapv select [reg jmp])
          eip (get registers :eip 0)]
      (if (> val 0)
        (assoc registers :eip (+ eip jmp))
        (assoc registers :eip (inc eip))))))

(defn eval-instruction [registers [tag & args] & channels]
(apply ({:MODIFY modify
         :SEND send-freq
         :RECOVER recover-freq
         :JUMP jump} tag)
       registers
       (into (vec args) channels)))

;; Puzzle 1
(defn part1 [path]
  (let [instructions (parse-instructions (parse-input path))]
    (loop [reg {:eip 0}]
      (if (or (< (reg :eip) 0)
              (>= (reg :eip) (count instructions))
              (some? (reg :rec)))
        (reg :rec)
        (let [new-reg (async/<!! (eval-instruction reg (nth instructions (reg :eip))))]
          (recur new-reg))))))

;; Puzzle 2
(defn program [path id in out]
  (let [instructions (parse-instructions (parse-input path))]
    (async/go
      (loop [reg {:eip 0 :p id :id id}]
        (if (or (< (reg :eip) 0)
                (>= (reg :eip) (count instructions)))
          (do (async/close! out)
              reg)
          (let [new-reg (async/<! (eval-instruction reg (nth instructions (reg :eip)) in out))]
            (recur new-reg)))))))

;; In and out are defined in terms of the first program's
;; channel order (program 0)
;; The argument order of the channels is then flipped
;; for the second program (program 1)
(defn part2 [path]
  (let [in-chan (async/chan 1e6)
        out-chan (async/chan 1e6)]
    (do (program path 0 in-chan out-chan)
        (program path 1 out-chan in-chan))))

(defn -main [& args])
