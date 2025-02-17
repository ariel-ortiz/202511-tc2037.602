(ns factorial)

(defn fact-v1
  [n]
  (if (zero? n)
    1
    (*' n
        (fact-v1 (dec n)))))

(fact-v1 0)
(fact-v1 1)
(fact-v1 3)
(fact-v1 5)
(fact-v1 20)
(fact-v1 1000)
