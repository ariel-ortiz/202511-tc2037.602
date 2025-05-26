(ns ex)

(defn intercambia
  [s]
  (if (< (count s) 2)
    nil
    (let [[a b & c] s]
      (concat [b] [a] c))))

(intercambia '[a b c d])

(defn collatz
  [n]
  (cond
    (= n 1) '(1)
    (even? n) (cons n (collatz (/ n 2)))
    :else (cons n (collatz (inc (* n 3))))))

(collatz 7)
