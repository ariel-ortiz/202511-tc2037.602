(ns parallelism)

(def n 150000)

; Problema 1

; Versión secuencial
; R = 1105792
; Run #1 7546.97
; Run #2 7537.97
; Run #3 7446.609
; Run #4 7425.88
; Run #5 7400.81
; Promedio: 7471.64

; Versión paralela
; R = 1105792
; Run #1 378.72
; Run #2 365.20
; Run #3 339.51
; Run #4 347.06
; Run #5 354.31
; Promedio: 356.96

; Sp = T1 / Tp
; p = 8
; S8 = 7471.64 / 356.96 = 20.93

(defn bits
  [x]
  (.bitCount (biginteger x)))

(defn fact-seq
  [n]
  (loop [i 2
         r 1]
    (if (> i n)
      (bits r)
      (recur (inc i)
             (*' r i)))))

; (time (fact-seq n))

(defn fact-ranges
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p))
                     [(inc n)])))

(defn fact-partial
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i)
             (*' r i)))))

(defn fact-par
  [n]
  (let [p (.availableProcessors (Runtime/getRuntime))]
    (bits (reduce *'
                  (pmap fact-partial
                        (fact-ranges n p))))))


;(time (fact-par n))

(/ (+ 378.72 365.20 339.51 347.06 354.31) 5)
