; Solución a los ejercicios del problemario #1

(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

; Problema 1
(defn gibibytes->bytes
  [x]
  (* x
     1024
     1024
     1024))

; Problema 2
(defn fahrenheit->celsius
  [f]
  (/ (* 5.0
        (- f
           32.0))
     9.0))

; Problema 3
(defn sign
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))

; Problema 4
(defn roots
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b)
                   (* 4 a c)))
        f (* 2 a)
        x1 (/ (+ d e)
              f)
        x2 (/ (- d e)
              f)]
    [x1 x2]))

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

(deftest test-fahrenheit->celsius
  (is (= 100.0 (fahrenheit->celsius 212.0)))
  (is (= 0.0 (fahrenheit->celsius 32.0)))
  (is (= -40.0 (fahrenheit->celsius -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(run-tests)
