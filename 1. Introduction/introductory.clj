; Solución a los ejercicios del problemario #1

(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

; Problema 1
(defn gibibytes->bytes
  [x]
  (* x 1024 1024 1024))

; Problema 2
(defn fahrenheit->celsius
  [f]
  (/ (* 5.0 (- f 32.0)) 9.0))

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

(deftest test-fahrenheit->celsius
  (is (= 100.0 (fahrenheit->celsius 212.0)))
  (is (= 0.0 (fahrenheit->celsius 32.0)))
  (is (= -40.0 (fahrenheit->celsius -40.0))))

(run-tests)
