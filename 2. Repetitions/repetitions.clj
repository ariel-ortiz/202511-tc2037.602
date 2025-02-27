(ns repetitions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  (:require [clojure.algo.generic.math-functions
             :refer [sqr approx=]]))

; Problema 1

; Versión recursiva
;(defn enlist
;  [s]
;  (if (empty? s)
;    ()
;    (cons (list (first s))
;          (enlist (rest s)))))

; Versión loop/recur
;(defn enlist
;  [s]
;  (loop [s s
;         result []]
;    (if (empty? s)
;      result
;      (recur (rest s)
;             (conj result
;                   (list (first s)))))))

; Versión API de secuencias
(defn enlist
  [s]
  (map list s))

; Problema 2
(defn positives
  [s]
  (filter pos? s))

; Problema 3
(defn add-squares
  [s]
  (reduce + (map sqr s)))


; Problema 4

; Versión recursiva
;(defn duplicate
;  [s]
;  (if (empty? s)
;    ()
;    (cons (first s)
;          (cons (first s)
;                (duplicate (rest s))))))

; Versión loop/recur
;(defn duplicate
;  [s]
;  (loop [new-s s
;         result []]
;    (if (empty? new-s)
;      result
;      (recur (rest new-s)
;             (conj (conj result
;                         (first new-s))
;                   (first new-s))))))

; Versión del API de secuencias
(defn duplicate
  [s]
  (interleave s s))

; Problema 5

; Versión recursiva
;(defn fib
;  [n]
;  (if (<= n 1)
;    n
;    (+ (fib (- n 1))
;       (fib (- n 2)))))

; Versión loop/recur
;(defn fib
;  [n]
;  (loop [a 0
;         b 1
;         i 0]
;    (if (= i n)
;      a
;      (recur b
;             (+' a b)
;             (inc i)))))

(defn aux
  [[a b]]
  [b (+' a b)])


; Versión API de secuencias
(defn fib
  [n]
  (first (first (drop n
                      (iterate aux [0 1])))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

(deftest test-positives
  (is (= () (positives ())))
  (is (= () (positives [-4 -1 -10 -13 -5])))
  (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
  (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))

(deftest test-add-squares
  (is (= 0 (add-squares [])))
  (is (= 25 (add-squares [5])))
  (is (= 30 (add-squares [2 4 1 3])))
  (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))

(deftest test-duplicate
  (is (= [1 1 2 2 3 3 4 4 5 5]
         (duplicate [1 2 3 4 5])))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
          987 1597 2584 4181 6765]
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

(run-tests)
