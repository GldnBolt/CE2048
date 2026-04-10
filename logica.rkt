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
(define m 8)
(define n 10)

(define origen-x 200)
(define origen-y 50)

(define ancho-tablero 400)
(define alto-tablero 400)

(define ancho-celda (/ ancho-tablero n))
(define alto-celda (/ alto-tablero m))

; CREACION DE LISTAS Y TABLERO
(define (crear-ceros cantidad)
  (cond
    [(zero? cantidad) '()]
    [else (cons 0 (crear-ceros (sub1 cantidad)))]))

(define (fila-vacia)
  (crear-ceros n))

(define (tablero-vacio)
  (tablero-vacio-aux m))

(define (tablero-vacio-aux filas)
  (cond
    [(zero? filas) '()]
    [else (cons (fila-vacia)
                (tablero-vacio-aux (sub1 filas)))]))

; ACCESO Y MODIFICACION
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

; POSICIONES VACIAS
(define (posiciones-vacias tablero)
  (posiciones-vacias-aux tablero 0 0))

(define (posiciones-vacias-aux tablero fil col)
  (cond
    [(= fil m) '()]
    [(= col n)
     (posiciones-vacias-aux tablero (add1 fil) 0)]
    [(= (valor-en tablero fil col) 0)
     (cons (list fil col)
           (posiciones-vacias-aux tablero fil (add1 col)))]
    [else
     (posiciones-vacias-aux tablero fil (add1 col))]))

(define (seleccionar-posicion-aleatoria posiciones)
  (cond
    [(null? posiciones) '()]
    [else (list-ref posiciones (random (length posiciones)))]))

(define (poner-en-posicion tablero posicion valor)
  (cond
    [(null? posicion) tablero]
    [else
     (poner-valor tablero
                  (car posicion)
                  (cadr posicion)
                  valor)]))

; GENERACION DE BALDOSAS
(define (agregar-2-inicial tablero)
  (poner-en-posicion
   tablero
   (seleccionar-posicion-aleatoria (posiciones-vacias tablero))
   2))

(define (nuevo-valor)
  (cond
    [(< (random 10) 9) 2]
    [else 4]))

(define (agregar-baldosa-aleatoria tablero)
  (poner-en-posicion
   tablero
   (seleccionar-posicion-aleatoria (posiciones-vacias tablero))
   (nuevo-valor)))

(define (tablero-inicial)
  (agregar-2-inicial
   (agregar-2-inicial
    (tablero-vacio))))

; UTILIDADES DE LISTAS
(define (invertir lista)
  (invertir-aux lista '()))

(define (invertir-aux lista acumulada)
  (cond
    [(null? lista) acumulada]
    [else
     (invertir-aux (cdr lista)
                   (cons (car lista) acumulada))]))

(define (quitar-ceros fila)
  (cond
    [(null? fila) '()]
    [(= (car fila) 0)
     (quitar-ceros (cdr fila))]
    [else
     (cons (car fila)
           (quitar-ceros (cdr fila)))]))

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
  (append fila
          (crear-ceros (- largo (length fila)))))

; MOVIMIENTO DE FILAS
(define (mover-fila-izquierda fila)
  (rellenar-con-ceros
   (fusionar (quitar-ceros fila))
   (length fila)))

(define (mover-fila-derecha fila)
  (invertir
   (mover-fila-izquierda
    (invertir fila))))

; MOVIMIENTO DE TABLEROS
(define (mover-izquierda tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (mover-fila-izquierda (car tablero))
           (mover-izquierda (cdr tablero)))]))

(define (mover-derecha tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (mover-fila-derecha (car tablero))
           (mover-derecha (cdr tablero)))]))

(define (primeros matriz)
  (cond
    [(null? matriz) '()]
    [else
     (cons (caar matriz)
           (primeros (cdr matriz)))]))

(define (restos matriz)
  (cond
    [(null? matriz) '()]
    [else
     (cons (cdar matriz)
           (restos (cdr matriz)))]))

(define (transponer matriz)
  (cond
    [(null? matriz) '()]
    [(null? (car matriz)) '()]
    [else
     (cons (primeros matriz)
           (transponer (restos matriz)))]))

(define (mover-arriba tablero)
  (transponer
   (mover-izquierda
    (transponer tablero))))

(define (mover-abajo tablero)
  (transponer
   (mover-derecha
    (transponer tablero))))

(define (mover-tablero tablero direccion)
  (cond
    [(eq? direccion 'left) (mover-izquierda tablero)]
    [(eq? direccion 'right) (mover-derecha tablero)]
    [(eq? direccion 'up) (mover-arriba tablero)]
    [(eq? direccion 'down) (mover-abajo tablero)]
    [else tablero]))

(define (mover-y-crear tablero direccion)
  (mover-y-crear-aux tablero (mover-tablero tablero direccion)))

(define (mover-y-crear-aux tablero nuevo-tablero)
  (cond
    [(equal? nuevo-tablero tablero) tablero]
    [else (agregar-baldosa-aleatoria nuevo-tablero)]))

; CONDICIONES DEL JUEGO
(define (fila-contiene? fila valor)
  (cond
    [(null? fila) #f]
    [(= (car fila) valor) #t]
    [else (fila-contiene? (cdr fila) valor)]))

(define (tablero-contiene? tablero valor)
  (cond
    [(null? tablero) #f]
    [(fila-contiene? (car tablero) valor) #t]
    [else (tablero-contiene? (cdr tablero) valor)]))

(define (gano? tablero)
  (tablero-contiene? tablero 2048))

(define (hay-vacias? tablero)
  (not (null? (posiciones-vacias tablero))))

(define (adyacentes-iguales-en-fila? fila)
  (cond
    [(null? fila) #f]
    [(null? (cdr fila)) #f]
    [(= (car fila) (cadr fila)) #t]
    [else (adyacentes-iguales-en-fila? (cdr fila))]))

(define (adyacentes-iguales-h? tablero)
  (cond
    [(null? tablero) #f]
    [(adyacentes-iguales-en-fila? (car tablero)) #t]
    [else (adyacentes-iguales-h? (cdr tablero))]))

(define (adyacentes-iguales-v? tablero)
  (adyacentes-iguales-h? (transponer tablero)))

(define (hay-movimientos? tablero)
  (or (hay-vacias? tablero)
      (adyacentes-iguales-h? tablero)
      (adyacentes-iguales-v? tablero)))

(define (perdio? tablero)
  (and (not (gano? tablero))
       (not (hay-movimientos? tablero))))