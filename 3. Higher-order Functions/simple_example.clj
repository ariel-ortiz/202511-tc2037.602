(ns simple-example)

(defn $map
  [fun seq]
  (if (empty? seq)
    ()
    (cons (fun (first seq))
          ($map fun (rest seq)))))

($map inc [4 8 15])
($map #(* % 2) [1 8 15 16 23 42])

(defn suma
  [base]
  (fn [x] (+ x base)))

(def z (suma 10))
(z 4)
(z 100)
(def w (suma 15))
(w 4)
(z 50)
(w 50)

((suma 20) 5)
