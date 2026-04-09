#lang racket

(provide m
         n
         ancho-celda
         alto-celda
         tablero-inicial
         mover-tablero
         mover-y-crear
         valor-en)

;TAMAÑO DEL TABLERO
(define m 4) ; filas
(define n 4) ; columnas

;MEDIDAS DE CADA CELDA
(define ancho-celda (/ 400 n))
(define alto-celda (/ 400 m))

;TABLERO VACIO
(define (fila-vacia)
  (make-list n 0))

(define (tablero-vacio)
  (build-list m (lambda (x) (fila-vacia))))

;REEMPLAZAR UN VALOR EN UNA LISTA
(define (reemplazar-en-lista lst pos valor)
  (cond
    [(null? lst) '()]
    [(zero? pos) (cons valor (cdr lst))]
    [else (cons (car lst)
                (reemplazar-en-lista (cdr lst) (sub1 pos) valor))]))

;PONER UN VALOR EN EL TABLERO
(define (poner-valor tablero fil col valor)
  (reemplazar-en-lista
   tablero
   fil
   (reemplazar-en-lista (list-ref tablero fil) col valor)))

;OBTENER VALOR DE UNA POSICION
(define (valor-en tablero fil col)
  (list-ref (list-ref tablero fil) col))

;BUSCAR POSICIONES VACIAS
(define (posiciones-vacias tablero)
  (for*/list ([fil (in-range m)]
              [col (in-range n)]
              #:when (= (valor-en tablero fil col) 0))
    (list fil col)))

;AGREGAR UN 2 EN UNA POSICION VACIA ALEATORIA
(define (agregar-2-aleatorio tablero)
  (define vacias (posiciones-vacias tablero))
  (if (null? vacias)
      tablero
      (let* ([p (list-ref vacias (random (length vacias)))]
             [fil (first p)]
             [col (second p)])
        (poner-valor tablero fil col 2))))

;TABLERO INICIAL CON DOS 2
(define (tablero-inicial)
  (agregar-2-aleatorio
   (agregar-2-aleatorio
    (tablero-vacio))))

;QUITAR CEROS DE UNA FILA
(define (quitar-ceros fila)
  (filter (lambda (x) (not (= x 0))) fila))

;FUSIONAR NUMEROS IGUALES UNA SOLA VEZ
;Ejemplo: (2 2 2 0 0) -> (4 2)
(define (fusionar fila)
  (cond
    [(null? fila) '()]
    [(null? (cdr fila)) fila]
    [(= (car fila) (cadr fila))
     (cons (+ (car fila) (cadr fila))
           (fusionar (cddr fila)))]
    [else
     (cons (car fila)
           (fusionar (cdr fila)))]))

;RELLENAR CON CEROS AL FINAL
(define (rellenar-con-ceros fila largo)
  (append fila (make-list (- largo (length fila)) 0)))

;MOVER UNA FILA A LA IZQUIERDA
(define (mover-fila-izquierda fila)
  (rellenar-con-ceros
   (fusionar (quitar-ceros fila))
   (length fila)))

;MOVER UNA FILA A LA DERECHA
(define (mover-fila-derecha fila)
  (reverse (mover-fila-izquierda (reverse fila))))

;TRANSPONER MATRIZ
(define (transponer matriz)
  (apply map list matriz))

;MOVER TABLERO SEGUN DIRECCION
(define (mover-izquierda tablero)
  (map mover-fila-izquierda tablero))

(define (mover-derecha tablero)
  (map mover-fila-derecha tablero))

(define (mover-arriba tablero)
  (transponer
   (map mover-fila-izquierda
        (transponer tablero))))

(define (mover-abajo tablero)
  (transponer
   (map mover-fila-derecha
        (transponer tablero))))

(define (mover-tablero tablero direccion)
  (cond
    [(eq? direccion 'left)  (mover-izquierda tablero)]
    [(eq? direccion 'right) (mover-derecha tablero)]
    [(eq? direccion 'up)    (mover-arriba tablero)]
    [(eq? direccion 'down)  (mover-abajo tablero)]
    [else tablero]))

;MOVER Y CREAR UN NUEVO 2 SOLO SI EL TABLERO CAMBIO
(define (mover-y-crear tablero direccion)
  (define nuevo-tablero (mover-tablero tablero direccion))
  (if (equal? nuevo-tablero tablero)
      tablero
      (agregar-2-aleatorio nuevo-tablero)))