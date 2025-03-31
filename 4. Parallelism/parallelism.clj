(ns parallelism)

; (def n 150000)

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

;(time (fact-seq n))
;(time (fact-par n))


; Problema 4
; n = 200,000
; p = 8

; Versión secuencial
; Run #1 T1 = 1628.39
; Run #2 T1 = 1622.13
; Run #3 T1 = 1605.27
; Run #4 T1 = 1625.96
; Run #5 T1 = 1625.71
; Promedio: 1621.49

; Versión paralela
; Run #1 T8 = 764.79
; Run #2 T8 = 701.14
; Run #3 T8 = 677.69
; Run #4 T8 = 767.81
; Run #5 T8 = 719.76
; Promedio: 726.23

; Sp = T1 / Tp
; p = 8
; S8 = 1621.49 / 726.23 = 2.23

(defn create-random-data
  [n]
  (repeatedly n #(rand-int 1000)))

(create-random-data 100)

(defn insertion-sort
  [s]
  (loop [new-s s
         r ()]
    (if (empty? new-s)
      r
      (let [x              (first new-s)
            [before after] (split-with #(< % x) r)]
        (recur (rest new-s)
               (concat before [x] after))))))

(defn merge-algorithm
  [a b]
  (loop [new-a a
         new-b b
         r     []]
    (cond
      (empty? new-a)
      (concat r new-b)

      (empty? new-b)
      (concat r new-a)

      (< (first new-a) (first new-b))
      (recur (rest new-a)
             new-b
             (conj r (first new-a)))

      :else
      (recur new-a
             (rest new-b)
             (conj r (first new-b))))))

(defn hibrid-sort-seq
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-algorithm (hibrid-sort-seq a)
                       (hibrid-sort-seq b)))))

(defn hibrid-sort-par
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm (pmap hibrid-sort-par splitted)))))


(def n 200000)
(def random-data (create-random-data n))
(apply <= random-data)
(println (apply <= (time (hibrid-sort-seq random-data))))
(println (apply <= (time (hibrid-sort-par random-data))))
