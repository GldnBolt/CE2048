#lang racket

(provide m
         n
         origen-x
         origen-y
         ancho-tablero
         alto-tablero
         ancho-celda
         alto-celda
         tablero-inicial
         mover-tablero
         mover-y-crear
         valor-en
         gano?
         perdio?)


; CONFIGURACION DEL TABLERO
; Cambio estos valores cuando quiera
(define m 4) ; filas
(define n 4) ; columnas

(define origen-x 200)
(define origen-y 50)

(define ancho-tablero 400)
(define alto-tablero 400)

(define ancho-celda (/ ancho-tablero n))
(define alto-celda (/ alto-tablero m))


; FUNCIONES BASICAS DE TABLERO
(define (fila-vacia)
  (make-list n 0))

(define (tablero-vacio)
  (build-list m (lambda (x) (fila-vacia))))

(define (reemplazar-en-lista lst pos valor)
  (cond
    [(null? lst) '()]
    [(zero? pos) (cons valor (cdr lst))]
    [else (cons (car lst)
                (reemplazar-en-lista (cdr lst) (sub1 pos) valor))]))

(define (poner-valor tablero fil col valor)
  (reemplazar-en-lista
   tablero
   fil
   (reemplazar-en-lista (list-ref tablero fil) col valor)))

(define (valor-en tablero fil col)
  (list-ref (list-ref tablero fil) col))

(define (posiciones-vacias tablero)
  (for*/list ([fil (in-range m)]
              [col (in-range n)]
              #:when (= (valor-en tablero fil col) 0))
    (list fil col)))


; GENERACION DE BALDOSAS
; Inicialmente solo deben aparecer 2
(define (agregar-2-inicial tablero)
  (define vacias (posiciones-vacias tablero))
  (if (null? vacias)
      tablero
      (let* ([p (list-ref vacias (random (length vacias)))]
             [fil (first p)]
             [col (second p)])
        (poner-valor tablero fil col 2))))


(define (nuevo-valor)
  (if (< (random 10) 9) 2 4)) 

(define (agregar-baldosa-aleatoria tablero)
  (define vacias (posiciones-vacias tablero))
  (if (null? vacias)
      tablero
      (let* ([p (list-ref vacias (random (length vacias)))]
             [fil (first p)]
             [col (second p)])
        (poner-valor tablero fil col (nuevo-valor)))))

(define (tablero-inicial)
  (agregar-2-inicial
   (agregar-2-inicial
    (tablero-vacio))))


; MOVIMIENTO Y FUSION
(define (quitar-ceros fila)
  (filter (lambda (x) (not (= x 0))) fila))

; Fusiona una sola vez por movimiento
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

(define (rellenar-con-ceros fila largo)
  (append fila (make-list (- largo (length fila)) 0)))

(define (mover-fila-izquierda fila)
  (rellenar-con-ceros
   (fusionar (quitar-ceros fila))
   (length fila)))

(define (mover-fila-derecha fila)
  (reverse (mover-fila-izquierda (reverse fila))))

(define (transponer matriz)
  (apply map list matriz))

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

(define (mover-y-crear tablero direccion)
  (define nuevo-tablero (mover-tablero tablero direccion))
  (if (equal? nuevo-tablero tablero)
      tablero
      (agregar-baldosa-aleatoria nuevo-tablero)))


; CONDICION DEl JUEGUSKI
(define (gano? tablero)
  (for/or ([fil (in-range m)])
    (for/or ([col (in-range n)])
      (= (valor-en tablero fil col) 2048))))

(define (hay-vacias? tablero)
  (not (null? (posiciones-vacias tablero))))

(define (adyacentes-iguales-h? tablero)
  (for*/or ([fil (in-range m)]
            [col (in-range (sub1 n))])
    (= (valor-en tablero fil col)
       (valor-en tablero fil (add1 col)))))

(define (adyacentes-iguales-v? tablero)
  (for*/or ([fil (in-range (sub1 m))]
            [col (in-range n)])
    (= (valor-en tablero fil col)
       (valor-en tablero (add1 fil) col))))

(define (hay-movimientos? tablero)
  (or (hay-vacias? tablero)
      (adyacentes-iguales-h? tablero)
      (adyacentes-iguales-v? tablero)))

(define (perdio? tablero)
  (and (not (gano? tablero))
       (not (hay-movimientos? tablero))))