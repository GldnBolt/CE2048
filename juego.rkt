#lang racket

(require (lib "graphics.ss" "graphics"))  ; Importa la librería de gráficos
(require "logica.rkt")  ; Importa la lógica del juego definida en 'logica.rkt'

(open-graphics)  ; Inicia el entorno gráfico

; VENTANAS
(define ventana (open-viewport "JUEGO 2048" 800 500))  ; Crea una ventana para el juego con un tamaño de 800x500 píxeles
(define oculta (open-pixmap "JUEGO 2048" 800 500))  ; Crea una "ventana oculta" para trabajar en el renderizado del juego

; AYUDAS GRAFICAS
(define (entero x)
  (inexact->exact (round x)))  ; Redondea el valor de x al número entero más cercano

; Función para mostrar la pantalla de bienvenida
(define (mostrar-bienvenida)
  (mostrar-bienvenida-aux (open-viewport "BIENVENIDOS" 430 250)))  ; Abre una ventana de bienvenida

; Mostrar mensaje de bienvenida con instrucciones en la pantalla
(define (mostrar-bienvenida-aux ventana2)
  ((draw-viewport ventana2) "magenta")  ; Fondo magenta
  ((draw-string ventana2) (make-posn 80 95) "BIENVENIDOS AL JUEGO 2048" "white")  ; Título en blanco
  ((draw-string ventana2) (make-posn 85 135) "Use las flechas para mover" "white")  ; Instrucciones en blanco
  (sleep 2)  ; Espera 2 segundos antes de cerrar la ventana
  (close-viewport ventana2))  ; Cierra la ventana de bienvenida

; Función general para mostrar un mensaje en una ventana
(define (mostrar-mensaje titulo color linea1 linea2)
  (mostrar-mensaje-aux (open-viewport titulo 380 180) titulo color linea1 linea2))

; Función auxiliar para mostrar un mensaje dentro de una ventana
(define (mostrar-mensaje-aux v titulo color linea1 linea2)
  ((draw-viewport v) color)  ; Establece el color de fondo de la ventana
  ((draw-string v) (make-posn 145 55) titulo "white")  ; Título en blanco
  ((draw-string v) (make-posn 45 95) linea1 "white")  ; Primera línea de mensaje
  ((draw-string v) (make-posn 35 125) linea2 "white")  ; Segunda línea de mensaje
  (sleep 3)  ; Muestra el mensaje por 3 segundos
  (close-viewport v))  ; Cierra la ventana con el mensaje

; Función para determinar el color de la celda según su valor
(define (color-celda valor)
  (cond
    [(= valor 0) "tan"]  ; Si la celda está vacía, es de color "tan"
    [(= valor 2) "white"]  ; Valor 2 = blanco
    [(= valor 4) "yellow"]  ; Valor 4 = amarillo
    [(= valor 8) "orange"]  ; Valor 8 = naranja
    [(= valor 16) "red"]  ; Valor 16 = rojo
    [(= valor 32) "magenta"]  ; Valor 32 = magenta
    [(= valor 64) "blue"]  ; Valor 64 = azul
    [(= valor 128) "cyan"]  ; Valor 128 = cyan
    [(= valor 256) "green"]  ; Valor 256 = verde
    [(= valor 512) "gray"]  ; Valor 512 = gris
    [(= valor 1024) "magenta"]  ; Valor 1024 = magenta
    [(= valor 2048) "green"]  ; Valor 2048 = verde
    [else "black"]))  ; Los demás valores son negros

; Función para determinar el color del texto según el valor de la celda
(define (color-texto valor)
  (cond
    [(<= valor 8) "black"]  ; Para valores bajos, texto negro
    [else "white"]))  ; Para valores altos, texto blanco

; Ajuste de la posición X para centrar el texto dentro de la celda
(define (ajuste-x valor)
  (cond
    [(< valor 10) (/ ancho-celda 2.4)]  ; Ajuste para valores pequeños
    [(< valor 100) (/ ancho-celda 3.2)]  ; Ajuste para valores medianos
    [(< valor 1000) (/ ancho-celda 4.2)]  ; Ajuste para valores grandes
    [else (/ ancho-celda 5.0)]))  ; Ajuste para valores muy grandes

; Función para verificar si un valor debe brillar (celda con valor >= 128)
(define (brillo? valor)
  (cond
    [(>= valor 128) #t]  ; Si el valor es mayor o igual a 128, la celda debe brillar
    [else #f]))  ; Si es menor, no brilla

; Función para determinar el color del brillo según el valor
(define (color-brillo valor)
  (cond
    [(= valor 128) "white"]
    [(= valor 256) "yellow"]
    [(= valor 512) "cyan"]
    [(= valor 1024) "magenta"]
    [(= valor 2048) "green"]
    [else "white"]))  ; Otros valores no tienen color de brillo específico

; DIBUJO

; Dibuja el fondo del tablero en la ventana oculta
(define (dibujar-fondo)
  ((draw-solid-rectangle oculta) (make-posn 0 0) 800 500 "white")  ; Dibuja un fondo blanco
  ((draw-solid-rectangle oculta)  ; Dibuja el área del tablero
   (make-posn (entero origen-x) (entero origen-y))
   (entero ancho-tablero)
   (entero alto-tablero)
   "tan"))  ; Color "tan" para el área del tablero

; Dibuja la información de la parte superior (instrucciones, tamaño del tablero, etc.)
(define (dibujar-info puntaje)
  ((draw-string oculta) (make-posn 20 25) "2048" "black")  ; Muestra el título "2048"
  ((draw-string oculta) (make-posn 20 50) "Use las flechas para mover las baldosas" "black")  ; Instrucciones
  ((draw-string oculta) (make-posn 20 75) "Presione Q para salir" "black")  ; Instrucción para salir
  ((draw-string oculta)  ; Muestra las dimensiones del tablero
   (make-posn 20 100)
   (string-append "Tablero: " (number->string m) "x" (number->string n))
   "black")
  ((draw-string oculta)  ; Muestra el puntaje del jugador
   (make-posn 20 125)
   (string-append "Puntaje: " (number->string puntaje))
   "black"))

; Dibuja el brillo en una celda específica
(define (dibujar-brillo valor x y)
  ((draw-solid-rectangle oculta)  ; Dibuja el brillo con un pequeño margen
   (make-posn (entero (+ x 1)) (entero (+ y 1)))
   (entero (- ancho-celda 2))
   (entero (- alto-celda 2))
   (color-brillo valor))  ; Usa el color correspondiente al valor de la celda
  ((draw-solid-rectangle oculta)  ; Dibuja un segundo brillo más pequeño
   (make-posn (entero (+ x 2)) (entero (+ y 2)))
   (entero (- ancho-celda 4))
   (entero (- alto-celda 4))
   (color-brillo valor)))  ; Usa el color correspondiente al valor de la celda

; Dibuja una celda específica en el tablero
(define (dibujar-celda col fil valor)
  (dibujar-celda-aux
   valor
   (+ origen-x (* col ancho-celda))
   (+ origen-y (* fil alto-celda))
   5))  ; Ajuste de margen para centrar el texto

; Función auxiliar para dibujar una celda con su valor y color
(define (dibujar-celda-aux valor x y margen)
  (cond
    [(brillo? valor)  ; Si la celda debe brillar, dibuja el brillo primero
     (dibujar-brillo valor x y)]
    [else #t])  ; Si no brilla, continúa sin hacer nada

  ((draw-solid-rectangle oculta)  ; Dibuja la celda
   (make-posn (entero (+ x margen)) (entero (+ y margen)))
   (entero (- ancho-celda (* 2 margen)))
   (entero (- alto-celda (* 2 margen)))
   (color-celda valor))  ; Color según el valor de la celda

  ((draw-rectangle oculta)  ; Dibuja el borde de la celda
   (make-posn (entero x) (entero y))
   (entero ancho-celda)
   (entero alto-celda)
   "black")

  (cond
    [(= valor 0) #t]  ; Si el valor es 0 (vacío), no dibuja el número
    [else
     ((draw-string oculta)  ; Dibuja el valor dentro de la celda
      (make-posn (entero (+ x (ajuste-x valor)))
                 (entero (+ y (/ alto-celda 1.6))))  ; Ajusta la posición para centrar el número
     (number->string valor)  ; Convierte el valor a string para mostrarlo
     (color-texto valor))]))  ; Determina el color del texto

; Dibuja el tablero completo
(define (dibujar-tablero tablero)
  (dibujar-filas tablero 0))  ; Llama a la función de filas para recorrer todo el tablero

; Dibuja las filas del tablero
(define (dibujar-filas filas fil)
  (cond
    [(null? filas) #t]  ; Si no hay más filas, termina
    [else
     (dibujar-columnas (car filas) fil 0)  ; Dibuja las columnas de la fila actual
     (dibujar-filas (cdr filas) (add1 fil))]))  ; Llama recursivamente para las siguientes filas

; Dibuja las columnas dentro de una fila
(define (dibujar-columnas fila fil col)
  (cond
    [(null? fila) #t]  ; Si no hay más columnas, termina
    [else
     (dibujar-celda col fil (car fila))  ; Dibuja la celda en la posición actual
     (dibujar-columnas (cdr fila) fil (add1 col))]))  ; Llama recursivamente para las siguientes columnas

; Redibuja todo el tablero
(define (redibujar tablero puntaje)
  (dibujar-fondo)  ; Dibuja el fondo
  (dibujar-info puntaje)  ; Dibuja la información superior
  (dibujar-tablero tablero)  ; Dibuja las celdas del tablero
  (copy-viewport oculta ventana))  ; Copia el contenido de la ventana oculta a la ventana visible

; TECLADO

; Verifica si la tecla presionada es una flecha
(define (flecha? tecla)
  (or (eq? tecla 'left)  ; Compara con las flechas
      (eq? tecla 'right)
      (eq? tecla 'up)
      (eq? tecla 'down)))  ; Si es una flecha, retorna #t

; BUCLE PRINCIPAL

; Bucle principal del juego, dibuja el tablero y procesa los eventos
(define (bucle-juego tablero puntaje)
  (redibujar tablero puntaje)  ; Redibuja el tablero
  (cond
    [(gano? tablero)  ; Si el jugador ganó
     (mostrar-mensaje "GANASTE" "green"
                      "Se detecto una baldosa con valor 2048."
                      (string-append "Puntaje: " (number->string puntaje)))
     (close-graphics)]  ; Cierra la ventana
    [(perdio? tablero)  ; Si el jugador perdió
     (mostrar-mensaje "FIN DEL JUEGO" "red"
                      "No quedan movimientos posibles."
                      (string-append "Puntaje: " (number->string puntaje)))
     (close-graphics)]  ; Cierra la ventana
    [else
     (procesar-tecla tablero puntaje (key-value (get-key-press ventana)))]))  ; Procesa la tecla presionada

; Procesa la tecla presionada por el jugador
(define (procesar-tecla tablero puntaje tecla)
  (cond
    [(flecha? tecla)  ; Si es una flecha, mueve las baldosas
     (procesar-resultado puntaje (mover-y-crear tablero tecla))]
    [(and (char? tecla)
          (or (char=? tecla #\q)  ; Si presiona Q, cierra el juego
              (char=? tecla #\Q)))
     (close-graphics)]  ; Cierra la ventana
    [else
     (bucle-juego tablero puntaje)]))  ; Si no es una tecla válida, sigue el bucle

; Procesa el resultado del movimiento y la puntuación
(define (procesar-resultado puntaje resultado)
  (bucle-juego
   (resultado-tablero resultado)  ; Actualiza el tablero
   (+ puntaje (resultado-puntos resultado))))  ; Acumula los puntos

; INICIO
(mostrar-bienvenida)  ; Muestra la pantalla de bienvenida
(bucle-juego (tablero-inicial) 0)  ; Inicia el bucle del juego con el tablero inicial