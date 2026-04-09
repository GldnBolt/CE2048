#lang racket

(provide m n ancho-celda alto-celda col1 fil1 col2 fil2)

;EJECUTANDO DIBUJO Y AZAR
;defino aca m y n para que siempre use el mismo
(define m 6)
(define n 5)

;LOGICA DE LOS NUMEROS 2
(define ancho-celda (/ 400 n))
(define alto-celda (/ 400 m))

;PRIMER 2
(define col1 (random n))
(define fil1 (random m))

;SEGUNDO 2
(define col2 (random n))
(define fil2 (random m))