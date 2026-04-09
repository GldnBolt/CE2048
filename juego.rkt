#lang racket
(require 2htdp/universe) ;efectos de movimiento
(require 2htdp/image) ;cargar imagenes
(require (lib "graphics.ss" "graphics")) ;libreria para graficos simples
(require "logica.rkt") ;archivo de logica
(open-graphics) ;abrir libreria


;DEFINICION DE VENTANAS
(define ventana (open-viewport "JUEGO 2048" 800 500)) ;ventana visible
(define oculta (open-pixmap "JUEGO 2048" 800 500)) ;ventana oculta para dibujar y luego copiar


;VENTANA DE BIENVENIDA
(define (mostrar-bienvenida)
  (define ventana2 (open-viewport "BIENVENIDOS" 400 250))
  (begin
    ((draw-viewport ventana2) "hotpink")
    ((draw-string ventana2) (make-posn 94 120) "BIENVENIDOS AL JUEGO 2048" "white")
    (sleep 3)
    (close-viewport ventana2)))


;DIBUJO DEL FONDO
(define (dibujar-fondo)
  ((draw-solid-rectangle oculta) (make-posn 0 0) 800 500 "ivory")
  ((draw-solid-rectangle oculta) (make-posn 200 50) 400 400 "tan"))


;FUNCION PARA EL TABLERO (las lineas)
(define (dibujar-tablero m n)
  (let ([ancho (/ 400 n)]
        [alto (/ 400 m)])
    (for ([i (in-range n)])
      (for ([j (in-range m)])
        ((draw-rectangle oculta)
         (make-posn (+ 200 (* i ancho))
                    (+ 50 (* j alto)))
         ancho
         alto
         "black")))))


;AJUSTE PARA CENTRAR MEJOR LOS NUMEROS
(define (ajuste-x valor)
  (cond
    [(< valor 10) (/ ancho-celda 2.5)]
    [(< valor 100) (/ ancho-celda 3.3)]
    [(< valor 1000) (/ ancho-celda 4.2)]
    [else (/ ancho-celda 5.2)]))

;DIBUJAR UN NUMERO EN UNA CELDA
(define (dibujar-numero col fil valor)
  ((draw-string oculta)
   (make-posn (+ 200 (* col ancho-celda) (ajuste-x valor))
              (+ 50 (* fil alto-celda) (/ alto-celda 1.5)))
   (number->string valor)
   "black"))

;DIBUJAR TODOS LOS NUMEROS DEL TABLERO
(define (dibujar-numeros tablero)
  (for ([fil (in-range m)])
    (for ([col (in-range n)])
      (define valor (valor-en tablero fil col))
      (when (not (= valor 0))
        (dibujar-numero col fil valor)))))


;REDIBUJAR TODO
(define (redibujar tablero)
  (dibujar-fondo)
  (dibujar-tablero m n)
  (dibujar-numeros tablero)
  (copy-viewport oculta ventana))


;REVISAR SI LA TECLA ES UNA FLECHA
(define (tecla-flecha? tecla)
  (or (eq? tecla 'left)
      (eq? tecla 'right)
      (eq? tecla 'up)
      (eq? tecla 'down)))


;BUCLE PRINCIPAL DEL JUEGO
(define (bucle-juego tablero)
  (redibujar tablero)
  (define tecla (key-value (get-key-press ventana)))
  (cond
    [(tecla-flecha? tecla)
     (bucle-juego (mover-y-crear tablero tecla))]
    [(and (char? tecla)
          (or (char=? tecla #\q)
              (char=? tecla #\Q)))
     (close-graphics)]
    [else
     (bucle-juego tablero)]))


;INICIO
(mostrar-bienvenida)
(define tablero (tablero-inicial))
(bucle-juego tablero)