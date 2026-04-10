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
(define m 8)  ; Definimos el número de filas del tablero (8 en este caso)
(define n 10)  ; Definimos el número de columnas del tablero (10 en este caso)

(define origen-x 200)  ; Coordenada X para el origen del tablero en la pantalla
(define origen-y 50)   ; Coordenada Y para el origen del tablero en la pantalla

(define ancho-tablero 400)  ; Ancho del tablero en píxeles
(define alto-tablero 400)   ; Alto del tablero en píxeles

(define ancho-celda (/ ancho-tablero n))  ; Ancho de cada celda en el tablero
(define alto-celda (/ alto-tablero m))   ; Alto de cada celda en el tablero

; ACCESORES DE RESULTADOS
(define (resultado-tablero resultado)
  (car resultado))  ; Retorna el tablero de la estructura de resultado

(define (resultado-puntos resultado)
  (cadr resultado))  ; Retorna los puntos acumulados de la estructura de resultado

; CREACION DE LISTAS Y TABLERO
(define (crear-ceros cantidad)
  (cond
    [(zero? cantidad) '()]  ; Si no hay más celdas que crear, retornamos una lista vacía
    [else (cons 0 (crear-ceros (sub1 cantidad)))]))  ; Agrega un 0 y continúa creando el resto de celdas

(define (fila-vacia)
  (crear-ceros n))  ; Crea una fila de n celdas vacías (todas con valor 0)

(define (tablero-vacio)
  (tablero-vacio-aux m))  ; Crea un tablero vacío con m filas

(define (tablero-vacio-aux filas)
  (cond
    [(zero? filas) '()]  ; Si no quedan filas por crear, retornamos una lista vacía
    [else
     (cons (fila-vacia)  ; Agrega una fila vacía
           (tablero-vacio-aux (sub1 filas)))]))  ; Llama recursivamente para agregar el resto de filas

; ACCESO Y MODIFICACION
(define (reemplazar-en-lista lst pos valor)
  (cond
    [(null? lst) '()]  ; Si la lista está vacía, no podemos reemplazar nada
    [(zero? pos) (cons valor (cdr lst))]  ; Si llegamos a la posición deseada, reemplazamos el valor
    [else
     (cons (car lst)
           (reemplazar-en-lista (cdr lst) (sub1 pos) valor))]))  ; Recursivamente buscamos la posición y reemplazamos

(define (poner-valor tablero fil col valor)
  (reemplazar-en-lista
   tablero
   fil  ; Reemplaza el valor en la fila especificada
   (reemplazar-en-lista (list-ref tablero fil) col valor)))  ; Reemplaza el valor en la columna especificada dentro de esa fila

(define (valor-en tablero fil col)
  (list-ref (list-ref tablero fil) col))  ; Obtiene el valor en la posición (fil, col) del tablero

; POSICIONES VACIAS
(define (posiciones-vacias tablero)
  (posiciones-vacias-aux tablero 0 0))  ; Busca todas las posiciones vacías en el tablero

(define (posiciones-vacias-aux tablero fil col)
  (cond
    [(= fil m) '()]  ; Si hemos recorrido todas las filas, retornamos una lista vacía
    [(= col n)
     (posiciones-vacias-aux tablero (add1 fil) 0)]  ; Si llegamos al final de una fila, pasamos a la siguiente
    [(= (valor-en tablero fil col) 0)  ; Si la celda está vacía
     (cons (list fil col)  ; Añadimos la posición (fila, columna) a la lista
           (posiciones-vacias-aux tablero fil (add1 col)))]  ; Continuamos buscando en la siguiente columna
    [else
     (posiciones-vacias-aux tablero fil (add1 col))]))  ; Si la celda no está vacía, pasamos a la siguiente columna

(define (seleccionar-posicion-aleatoria posiciones)
  (cond
    [(null? posiciones) '()]  ; Si no hay posiciones vacías, retornamos una lista vacía
    [else (list-ref posiciones (random (length posiciones)))]))  ; Seleccionamos una posición aleatoria

(define (poner-en-posicion tablero posicion valor)
  (cond
    [(null? posicion) tablero]  ; Si no hay posición, retornamos el tablero sin cambios
    [else
     (poner-valor tablero
                  (car posicion)  ; Reemplazamos el valor en la fila indicada por la posición
                  (cadr posicion)  ; Reemplazamos el valor en la columna indicada por la posición
                  valor)]))  ; Colocamos el valor en la posición deseada

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