#lang racket

(require (lib "graphics.ss" "graphics"))
(require "logica.rkt")

(open-graphics)

; VENTANAS
(define ventana (open-viewport "JUEGO 2048" 800 500))
(define oculta (open-pixmap "JUEGO 2048" 800 500))

; AYUDAS GRAFICAS
(define (entero x)
  (inexact->exact (round x)))

(define (mostrar-bienvenida)
  (mostrar-bienvenida-aux (open-viewport "BIENVENIDOS" 430 250)))

(define (mostrar-bienvenida-aux ventana2)
  ((draw-viewport ventana2) "magenta")
  ((draw-string ventana2) (make-posn 80 95) "BIENVENIDOS AL JUEGO 2048" "white")
  ((draw-string ventana2) (make-posn 85 135) "Use las flechas para mover" "white")
  (sleep 2)
  (close-viewport ventana2))

(define (mostrar-mensaje titulo color linea1 linea2)
  (mostrar-mensaje-aux (open-viewport titulo 380 180) titulo color linea1 linea2))

(define (mostrar-mensaje-aux v titulo color linea1 linea2)
  ((draw-viewport v) color)
  ((draw-string v) (make-posn 145 55) titulo "white")
  ((draw-string v) (make-posn 45 95) linea1 "white")
  ((draw-string v) (make-posn 35 125) linea2 "white")
  (sleep 3)
  (close-viewport v))

(define (color-celda valor)
  (cond
    [(= valor 0) "tan"]
    [(= valor 2) "white"]
    [(= valor 4) "yellow"]
    [(= valor 8) "orange"]
    [(= valor 16) "red"]
    [(= valor 32) "magenta"]
    [(= valor 64) "blue"]
    [(= valor 128) "cyan"]
    [(= valor 256) "green"]
    [(= valor 512) "gray"]
    [(= valor 1024) "magenta"]
    [(= valor 2048) "green"]
    [else "black"]))

(define (color-texto valor)
  (cond
    [(<= valor 8) "black"]
    [else "white"]))

(define (ajuste-x valor)
  (cond
    [(< valor 10) (/ ancho-celda 2.4)]
    [(< valor 100) (/ ancho-celda 3.2)]
    [(< valor 1000) (/ ancho-celda 4.2)]
    [else (/ ancho-celda 5.0)]))

(define (brillo? valor)
  (cond
    [(>= valor 128) #t]
    [else #f]))

(define (color-brillo valor)
  (cond
    [(= valor 128) "white"]
    [(= valor 256) "yellow"]
    [(= valor 512) "cyan"]
    [(= valor 1024) "magenta"]
    [(= valor 2048) "green"]
    [else "white"]))

; DIBUJO
(define (dibujar-fondo)
  ((draw-solid-rectangle oculta) (make-posn 0 0) 800 500 "white")
  ((draw-solid-rectangle oculta)
   (make-posn (entero origen-x) (entero origen-y))
   (entero ancho-tablero)
   (entero alto-tablero)
   "tan"))

(define (dibujar-info puntaje)
  ((draw-string oculta) (make-posn 20 25) "2048" "black")
  ((draw-string oculta) (make-posn 20 50) "Use las flechas para mover las baldosas" "black")
  ((draw-string oculta) (make-posn 20 75) "Presione Q para salir" "black")
  ((draw-string oculta)
   (make-posn 20 100)
   (string-append "Tablero: " (number->string m) "x" (number->string n))
   "black")
  ((draw-string oculta)
   (make-posn 20 125)
   (string-append "Puntaje: " (number->string puntaje))
   "black"))

(define (dibujar-brillo valor x y)
  ((draw-solid-rectangle oculta)
   (make-posn (entero (+ x 1)) (entero (+ y 1)))
   (entero (- ancho-celda 2))
   (entero (- alto-celda 2))
   (color-brillo valor))
  ((draw-solid-rectangle oculta)
   (make-posn (entero (+ x 2)) (entero (+ y 2)))
   (entero (- ancho-celda 4))
   (entero (- alto-celda 4))
   (color-brillo valor)))

(define (dibujar-celda col fil valor)
  (dibujar-celda-aux
   valor
   (+ origen-x (* col ancho-celda))
   (+ origen-y (* fil alto-celda))
   5))

(define (dibujar-celda-aux valor x y margen)
  (cond
    [(brillo? valor)
     (dibujar-brillo valor x y)]
    [else #t])

  ((draw-solid-rectangle oculta)
   (make-posn (entero (+ x margen)) (entero (+ y margen)))
   (entero (- ancho-celda (* 2 margen)))
   (entero (- alto-celda (* 2 margen)))
   (color-celda valor))

  ((draw-rectangle oculta)
   (make-posn (entero x) (entero y))
   (entero ancho-celda)
   (entero alto-celda)
   "black")

  (cond
    [(= valor 0) #t]
    [else
     ((draw-string oculta)
      (make-posn (entero (+ x (ajuste-x valor)))
                 (entero (+ y (/ alto-celda 1.6))))
      (number->string valor)
      (color-texto valor))]))

(define (dibujar-tablero tablero)
  (dibujar-filas tablero 0))

(define (dibujar-filas filas fil)
  (cond
    [(null? filas) #t]
    [else
     (dibujar-columnas (car filas) fil 0)
     (dibujar-filas (cdr filas) (add1 fil))]))

(define (dibujar-columnas fila fil col)
  (cond
    [(null? fila) #t]
    [else
     (dibujar-celda col fil (car fila))
     (dibujar-columnas (cdr fila) fil (add1 col))]))

(define (redibujar tablero puntaje)
  (dibujar-fondo)
  (dibujar-info puntaje)
  (dibujar-tablero tablero)
  (copy-viewport oculta ventana))

; TECLADO
(define (flecha? tecla)
  (or (eq? tecla 'left)
      (eq? tecla 'right)
      (eq? tecla 'up)
      (eq? tecla 'down)))

; BUCLE PRINCIPAL
(define (bucle-juego tablero puntaje)
  (redibujar tablero puntaje)
  (cond
    [(gano? tablero)
     (mostrar-mensaje "GANASTE" "green"
                      "Se detecto una baldosa con valor 2048."
                      (string-append "Puntaje: " (number->string puntaje)))
     (close-graphics)]
    [(perdio? tablero)
     (mostrar-mensaje "FIN DEL JUEGO" "red"
                      "No quedan movimientos posibles."
                      (string-append "Puntaje: " (number->string puntaje)))
     (close-graphics)]
    [else
     (procesar-tecla tablero puntaje (key-value (get-key-press ventana)))]))

(define (procesar-tecla tablero puntaje tecla)
  (cond
    [(flecha? tecla)
     (procesar-resultado puntaje (mover-y-crear tablero tecla))]
    [(and (char? tecla)
          (or (char=? tecla #\q)
              (char=? tecla #\Q)))
     (close-graphics)]
    [else
     (bucle-juego tablero puntaje)]))

(define (procesar-resultado puntaje resultado)
  (bucle-juego
   (resultado-tablero resultado)
   (+ puntaje (resultado-puntos resultado))))

; INICIO
(mostrar-bienvenida)
(bucle-juego (tablero-inicial) 0)