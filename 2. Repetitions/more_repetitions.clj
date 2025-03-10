(ns more-repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))

; Problema 1
(defn expand
  [s]
  (mapcat repeat
          (range 1 (inc (count s)))
          s))

; Problema 2

; Versión recursiva
;(defn insert
;  [n s]
;  (cond
;    (empty? s)        (list n)
;    (<= n (first s))  (cons n s)
;    :else             (cons (first s)
;                            (insert n (rest s)))))

; Versión loop/recur
;(defn insert
;  [n s]
;  (loop [new-s s
;         accum []]
;    (cond
;      (empty? new-s)        (conj accum n)
;      (<= n (first new-s))  (concat (conj accum n)
;                                    new-s)
;      :else                 (recur (rest new-s)
;                                   (conj accum (first new-s))))))

; Versión API de secuencias
(defn insert
  [n s]
  (let [[a b] (split-with #(< % n) s)]
    (concat a [n] b)))

; Problema 3

; Versión recursiva
;(defn insertion-sort
;  [s]
;  (if (empty? s)
;     ()
;     (insert (first s)
;             (insertion-sort (rest s)))))

; Versión API de secuencias
(defn insertion-sort
  [s]
  (reduce #(insert %2 %1) () s))

; Problema 5

; Versión loop/recur
;(defn binary
;  [n]
;  (loop [new-n   n
;         result  ()]
;    (if (zero? new-n)
;      result
;      (recur (quot new-n 2)
;             (cons (rem new-n 2) result)))))

; Versión API de secuencias
(defn binary
  [n]
  (second
    (first
      (drop-while
        (fn [[n _result]]
          (not (zero? n)))
        (iterate (fn [[n result]]
                   [(quot n 2)
                    (cons (rem n 2) result)])
                 [n ()])))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(run-tests)
