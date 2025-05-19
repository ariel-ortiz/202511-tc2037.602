(ns analisis-lexico)

; (seq (slurp "entrada.txt"))

; (spit "salida.txt" "¡Hola, mundo!")

(def er #"(?xi)
       ( -? \d+ \. \d* (?: e -? \d+)? )  # Grupo 1: Flotante
     | ( \d+ )                           # Grupo 2: Entero
     | ( // .*     )                     # Grupo 3: Comentario
     | ( [a-z] \w* )                     # Grupo 4: Variables
     | ( [=] )                           # Grupo 5: Asignación
     | ( [+] )                           # Grupo 6: Suma
     | ( [-] )                           # Grupo 7: Resta
     | ( [*] )                           # Grupo 8: Multiplicación
     | ( [/] )                           # Grupo 9: División
     | ( \^ )                            # Grupo 10: Potencia
     | ( [(] )                           # Grupo 11: Paréntesis que abre
     | ( [)] )                           # Grupo 12: Paréntesis que cierra
     | ( \s )                            # Grupo 13: Espacios
     | ( . )                             # Grupo 14: Carácter inválido
")

(defn tokenize
  [input]
  (remove #(nil? %)
          (map (fn [token]
                   (cond
                     (token 1) [:flotante (token 0)]
                     (token 2) [:entero (token 0)]
                     (token 3) [:comentario (token 0)]
                     (token 4) [:variable (token 0)]
                     (token 5) [:asignacion (token 0)]
                     (token 6) [:suma (token 0)]
                     (token 7) [:resta (token 0)]
                     (token 8) [:multiplicacion (token 0)]
                     (token 9) [:division (token 0)]
                     (token 10) [:potencia (token 0)]
                     (token 11) [:par-izq (token 0)]
                     (token 12) [:par-der (token 0)]
                     (token 14) [:error (token 0)]))
               (re-seq er input))))

(defn tokenize-file
  [file-name]
  (tokenize (slurp file-name)))


(tokenize-file "entrada.txt")
