#lang racket

(provide crear-estado-inicial
         mover-estado
         estado-tablero
         estado-puntaje
         estado-ganado
         estado-perdido
         crear-fila-vacia
         crear-tablero-vacio
         cantidad-filas
         cantidad-columnas
         obtener-en-lista
         obtener-celda
         reemplazar-en-lista
         reemplazar-celda
         posiciones-vacias
         insertar-baldosa
         insertar-baldosa-aleatoria
         contiene-2048?)

(struct estado (tablero puntaje ganado perdido) #:transparent)

;------------------------------------------------------------
; FUNCIONES BÁSICAS DE LISTAS
;------------------------------------------------------------

; obtener-en-lista: lista num -> any
; Retorna el elemento en la posición indice de una lista.
(define (obtener-en-lista lista indice)
  (cond
    [(null? lista) (error "Índice fuera de rango")]
    [(zero? indice) (car lista)]
    [else (obtener-en-lista (cdr lista) (- indice 1))]))

; reemplazar-en-lista: lista num any -> lista
; Retorna una nueva lista con el valor reemplazado en la posición indice.
(define (reemplazar-en-lista lista indice nuevo-valor)
  (cond
    [(null? lista) (error "Índice fuera de rango")]
    [(zero? indice) (cons nuevo-valor (cdr lista))]
    [else
     (cons (car lista)
           (reemplazar-en-lista (cdr lista) (- indice 1) nuevo-valor))]))

; longitud-lista: lista -> num
; Calcula la longitud de una lista de forma recursiva.
(define (longitud-lista lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (longitud-lista (cdr lista)))]))

;------------------------------------------------------------
; CREACIÓN DEL TABLERO
;------------------------------------------------------------

; crear-fila-vacia: num -> lista
; Crea una fila con columnas celdas en 0.
(define (crear-fila-vacia columnas)
  (cond
    [(zero? columnas) '()]
    [else (cons 0 (crear-fila-vacia (- columnas 1)))]))

; crear-tablero-vacio: num num -> tablero
; Crea un tablero vacío de filas x columnas.
(define (crear-tablero-vacio filas columnas)
  (cond
    [(zero? filas) '()]
    [else
     (cons (crear-fila-vacia columnas)
           (crear-tablero-vacio (- filas 1) columnas))]))

; cantidad-filas: tablero -> num
(define (cantidad-filas tablero)
  (longitud-lista tablero))

; cantidad-columnas: tablero -> num
(define (cantidad-columnas tablero)
  (cond
    [(null? tablero) 0]
    [else (longitud-lista (car tablero))]))

;------------------------------------------------------------
; ACCESO Y MODIFICACIÓN DEL TABLERO
;------------------------------------------------------------

; obtener-celda: tablero num num -> num
; Retorna el valor en la posición fila, columna.
(define (obtener-celda tablero fila columna)
  (obtener-en-lista (obtener-en-lista tablero fila) columna))

; reemplazar-celda: tablero num num num -> tablero
; Retorna un nuevo tablero con una celda reemplazada.
(define (reemplazar-celda tablero fila columna nuevo-valor)
  (reemplazar-en-lista
   tablero
   fila
   (reemplazar-en-lista (obtener-en-lista tablero fila) columna nuevo-valor)))

; insertar-baldosa: tablero num num num -> tablero
; Inserta un valor en una posición específica.
(define (insertar-baldosa tablero fila columna valor)
  (reemplazar-celda tablero fila columna valor))

;------------------------------------------------------------
; BÚSQUEDA DE POSICIONES VACÍAS
;------------------------------------------------------------

; posiciones-vacias-fila: lista num num -> lista
; Busca espacios vacíos en una fila.
(define (posiciones-vacias-fila fila indice-fila indice-columna)
  (cond
    [(null? fila) '()]
    [(zero? (car fila))
     (cons (list indice-fila indice-columna)
           (posiciones-vacias-fila (cdr fila) indice-fila (+ indice-columna 1)))]
    [else
     (posiciones-vacias-fila (cdr fila) indice-fila (+ indice-columna 1))]))

; posiciones-vacias-aux: tablero num -> lista
; Busca espacios vacíos en todo el tablero.
(define (posiciones-vacias-aux tablero indice-fila)
  (cond
    [(null? tablero) '()]
    [else
     (append (posiciones-vacias-fila (car tablero) indice-fila 0)
             (posiciones-vacias-aux (cdr tablero) (+ indice-fila 1)))]))

; posiciones-vacias: tablero -> lista
(define (posiciones-vacias tablero)
  (posiciones-vacias-aux tablero 0))

;------------------------------------------------------------
; INSERCIÓN ALEATORIA
;------------------------------------------------------------

; obtener-posicion-aleatoria: lista -> any
; Escoge un elemento aleatorio de una lista no vacía.
(define (obtener-posicion-aleatoria lista)
  (obtener-en-lista lista (random (longitud-lista lista))))

; valor-nueva-baldosa: -> num
; Retorna 2 o 4. Aquí dejamos 90% probabilidad para 2 y 10% para 4.
(define (valor-nueva-baldosa)
  (cond
    [(< (random 10) 9) 2]
    [else 4]))

; insertar-baldosa-aleatoria: tablero -> tablero
(define (insertar-baldosa-aleatoria tablero)
  (cond
    [(null? (posiciones-vacias tablero)) tablero]
    [else
     (insertar-baldosa-en-posicion
      tablero
      (obtener-posicion-aleatoria (posiciones-vacias tablero))
      (valor-nueva-baldosa))]))
;------------------------------------------------------------
; VERIFICACIÓN DE VICTORIA
;------------------------------------------------------------

; contiene-2048-fila?: lista -> boolean
(define (contiene-2048-fila? fila)
  (cond
    [(null? fila) #f]
    [(= (car fila) 2048) #t]
    [else (contiene-2048-fila? (cdr fila))]))

; contiene-2048?: tablero -> boolean
(define (contiene-2048? tablero)
  (cond
    [(null? tablero) #f]
    [(contiene-2048-fila? (car tablero)) #t]
    [else (contiene-2048? (cdr tablero))]))

;------------------------------------------------------------
; ESTADO INICIAL
;------------------------------------------------------------

; crear-estado-inicial: num num -> estado
; Crea un estado inicial con dos baldosas de valor 2.
(define (crear-estado-inicial filas columnas)
  (estado
   (insertar-dos-baldosas-iniciales (crear-tablero-vacio filas columnas))
   0
   #f
   #f))

; insertar-dos-baldosas-iniciales: tablero -> tablero
; Inserta dos baldosas iniciales de valor 2 en posiciones aleatorias.
(define (insertar-dos-baldosas-iniciales tablero)
  (insertar-dos-baldosas-iniciales-aux tablero 2))

; insertar-dos-baldosas-iniciales-aux: tablero num -> tablero
(define (insertar-dos-baldosas-iniciales-aux tablero cantidad)
  (cond
    [(zero? cantidad) tablero]
    [else
     (insertar-dos-baldosas-iniciales-aux
      (insertar-baldosa-aleatoria-con-valor tablero 2)
      (- cantidad 1))]))

; insertar-baldosa-aleatoria-con-valor: tablero num -> tablero
(define (insertar-baldosa-aleatoria-con-valor tablero valor)
  (cond
    [(null? (posiciones-vacias tablero)) tablero]
    [else
     (insertar-baldosa-en-posicion tablero
                                   (obtener-posicion-aleatoria (posiciones-vacias tablero))
                                   valor)]))

; insertar-baldosa-en-posicion: tablero posicion num -> tablero
(define (insertar-baldosa-en-posicion tablero posicion valor)
  (insertar-baldosa tablero
                    (car posicion)
                    (car (cdr posicion))
                    valor))

;------------------------------------------------------------
; TEMPORAL: mover-estado se implementa en la siguiente fase
;------------------------------------------------------------

(define (mover-estado est direccion)
  est)