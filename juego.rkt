#lang racket
(require 2htdp/universe) ;efectos de movimiento
(require 2htdp/image) ;cargar imagenes
(require (lib "graphics.ss" "graphics")) ;libreria para graficos simples
(open-graphics) ;abir libreria


;DEFINICION DE VENTANAS
(define ventana (open-viewport "JUEGO 2048" 800 500)) ;para ventana tipo viewport
(define oculta (open-pixmap "JUEGO 2048" 800 500)) ; lo mismo pero esta no se ve, es donde estan las operaciones y luego se reflejan en la viewport

;DIBUJO DEL FONDO (se hace primero para que este atras)
((draw-solid-rectangle oculta) (make-posn 0 0) 800 500 "ivory")
((draw-solid-rectangle oculta) (make-posn 200 50) 400 400 "tan")

;VENTANA DE BIENVENIDA
;necesito poner este "contenedor" para que ejecute varias instrucciones una tras otra para que parezcan una sola, lo hago para que pueda meterle varios parametros a la letra por ejemplo
(define ventana2 (open-viewport "BIENVENIDOS" 400 250))
   (begin
     ((draw-viewport ventana2) "hotpink")
     ((draw-string ventana2) (make-posn 94 120) "BIENVENIDOS AL JUEGO 2048" "white")
   (sleep 3)
   (close-viewport ventana2)
    )

; FUNCION PARA EL TABLERO (las lineas)
; n es el ancho que me dice cuantas columnas van a haber y m cuantas filas van a haber
(define (dibujar-tablero m n)
  (let ([ancho (/ 400 n)]
        [alto (/ 400 m)])
    (for ([i (in-range n)])
      (for ([j (in-range m)])
        ((draw-rectangle oculta) (make-posn (+ 200 (* i ancho)) (+ 50 (* j alto))) ancho alto "black")))))

;EJECUTANDO DIBUJO Y AZAR
;defino aca m y n para que siempre use el mismo y aparte llamo a la funcion que dibuja
(define m 6)
(define n 5)
(dibujar-tablero m n)


;LOGICA DE LOS MUMEROS 2
(define ancho-celda (/ 400 n)) ;tengo que hacerlas de nuevo porque las otras eran privadas para la funcion de dibujar tablero
(define alto-celda (/ 400 m))

;PRIMER 2
(define col1 (random n))
(define fil1 (random m))
((draw-string oculta) (make-posn(+ 200 (* col1 ancho-celda) (/ ancho-celda 2.5)) (+ 50 (* fil1 alto-celda) (/ alto-celda 1.5)))"2" "black")


;SEGUNDO 2
(define col2 (random n))
(define fil2 (random m))
((draw-string oculta) (make-posn(+ 200 (* col2 ancho-celda) (/ ancho-celda 2.5)) (+ 50 (* fil2 alto-celda) (/ alto-celda 1.5)))"2" "black")



(copy-viewport oculta ventana)
