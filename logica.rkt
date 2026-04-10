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
         resultado-tablero
         resultado-puntos
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

; ACCESORES DE RESULTADOS
(define (resultado-tablero resultado)
  (car resultado))

(define (resultado-puntos resultado)
  (cadr resultado))

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
    [else
     (cons (fila-vacia)
           (tablero-vacio-aux (sub1 filas)))]))

; ACCESO Y MODIFICACION
(define (reemplazar-en-lista lst pos valor)
  (cond
    [(null? lst) '()]
    [(zero? pos) (cons valor (cdr lst))]
    [else
     (cons (car lst)
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

(define (rellenar-con-ceros fila largo)
  (append fila
          (crear-ceros (- largo (length fila)))))

; FUSION Y PUNTAJE
(define (fusionar-y-puntuar fila)
  (cond
    [(null? fila) (list '() 0)]
    [(null? (cdr fila)) (list fila 0)]
    [(= (car fila) (cadr fila))
     (fusionar-y-puntuar-union
      (+ (car fila) (cadr fila))
      (fusionar-y-puntuar (cddr fila)))]
    [else
     (fusionar-y-puntuar-normal
      (car fila)
      (fusionar-y-puntuar (cdr fila)))]))

(define (fusionar-y-puntuar-union suma resultado-resto)
  (list
   (cons suma (resultado-tablero resultado-resto))
   (+ suma (resultado-puntos resultado-resto))))

(define (fusionar-y-puntuar-normal primero resultado-resto)
  (list
   (cons primero (resultado-tablero resultado-resto))
   (resultado-puntos resultado-resto)))

; MOVIMIENTO DE FILAS
(define (mover-fila-izquierda fila)
  (resultado-tablero (mover-fila-izquierda-resultado fila)))

(define (mover-fila-izquierda-resultado fila)
  (mover-fila-izquierda-resultado-aux
   fila
   (fusionar-y-puntuar (quitar-ceros fila))))

(define (mover-fila-izquierda-resultado-aux fila resultado)
  (list
   (rellenar-con-ceros
    (resultado-tablero resultado)
    (length fila))
   (resultado-puntos resultado)))

(define (mover-fila-derecha fila)
  (resultado-tablero (mover-fila-derecha-resultado fila)))

(define (mover-fila-derecha-resultado fila)
  (mover-fila-derecha-resultado-aux
   (mover-fila-izquierda-resultado (invertir fila))))

(define (mover-fila-derecha-resultado-aux resultado)
  (list
   (invertir (resultado-tablero resultado))
   (resultado-puntos resultado)))

; MOVIMIENTO DE TABLEROS
(define (mover-izquierda tablero)
  (resultado-tablero (mover-izquierda-resultado tablero)))

(define (mover-izquierda-resultado tablero)
  (cond
    [(null? tablero) (list '() 0)]
    [else
     (combinar-resultados-tablero
      (mover-fila-izquierda-resultado (car tablero))
      (mover-izquierda-resultado (cdr tablero)))]))

(define (mover-derecha tablero)
  (resultado-tablero (mover-derecha-resultado tablero)))

(define (mover-derecha-resultado tablero)
  (cond
    [(null? tablero) (list '() 0)]
    [else
     (combinar-resultados-tablero
      (mover-fila-derecha-resultado (car tablero))
      (mover-derecha-resultado (cdr tablero)))]))

(define (combinar-resultados-tablero resultado-fila resultado-resto)
  (list
   (cons (resultado-tablero resultado-fila)
         (resultado-tablero resultado-resto))
   (+ (resultado-puntos resultado-fila)
      (resultado-puntos resultado-resto))))

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

(define (transponer-resultado resultado)
  (list
   (transponer (resultado-tablero resultado))
   (resultado-puntos resultado)))

(define (mover-arriba tablero)
  (resultado-tablero (mover-arriba-resultado tablero)))

(define (mover-arriba-resultado tablero)
  (transponer-resultado
   (mover-izquierda-resultado
    (transponer tablero))))

(define (mover-abajo tablero)
  (resultado-tablero (mover-abajo-resultado tablero)))

(define (mover-abajo-resultado tablero)
  (transponer-resultado
   (mover-derecha-resultado
    (transponer tablero))))

(define (mover-tablero-resultado tablero direccion)
  (cond
    [(eq? direccion 'left) (mover-izquierda-resultado tablero)]
    [(eq? direccion 'right) (mover-derecha-resultado tablero)]
    [(eq? direccion 'up) (mover-arriba-resultado tablero)]
    [(eq? direccion 'down) (mover-abajo-resultado tablero)]
    [else (list tablero 0)]))

(define (mover-tablero tablero direccion)
  (resultado-tablero
   (mover-tablero-resultado tablero direccion)))

(define (mover-y-crear tablero direccion)
  (mover-y-crear-aux tablero (mover-tablero-resultado tablero direccion)))

(define (mover-y-crear-aux tablero resultado)
  (cond
    [(equal? (resultado-tablero resultado) tablero)
     (list tablero 0)]
    [else
     (list
      (agregar-baldosa-aleatoria (resultado-tablero resultado))
      (resultado-puntos resultado))]))

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