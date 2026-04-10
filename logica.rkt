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
   (seleccionar-posicion-aleatoria (posiciones-vacias tablero))  ; Coloca un 2 en una posición vacía aleatoria
   2))  ; El valor de la baldosa inicial es 2

(define (nuevo-valor)
  (cond
    [(< (random 10) 9) 2]  ; Genera un 2 con un 90% de probabilidad
    [else 4]))  ; Genera un 4 con un 10% de probabilidad

(define (agregar-baldosa-aleatoria tablero)
  (poner-en-posicion
   tablero
   (seleccionar-posicion-aleatoria (posiciones-vacias tablero))  ; Coloca una baldosa nueva en una posición vacía aleatoria
   (nuevo-valor)))  ; El valor de la nueva baldosa será 2 o 4

(define (tablero-inicial)
  (agregar-2-inicial  ; Coloca dos baldosas de valor 2 en el tablero vacío
   (agregar-2-inicial
    (tablero-vacio))))  ; Llama a la función para crear el tablero vacío

; UTILIDADES DE LISTAS
(define (invertir lista)
  (invertir-aux lista '()))  ; Invierte una lista utilizando recursión

(define (invertir-aux lista acumulada)
  (cond
    [(null? lista) acumulada]  ; Si la lista está vacía, retornamos la acumulada
    [else
     (invertir-aux (cdr lista)
                   (cons (car lista) acumulada))]))  ; De lo contrario, agregamos el primer elemento a la acumulada y continuamos

(define (quitar-ceros fila)
  (cond
    [(null? fila) '()]  ; Si la fila está vacía, retornamos una lista vacía
    [(= (car fila) 0)  ; Si el primer valor es 0, lo eliminamos
     (quitar-ceros (cdr fila))]  ; Llamamos recursivamente para el resto de la fila
    [else
     (cons (car fila)
           (quitar-ceros (cdr fila)))]))  ; Si el valor no es 0, lo dejamos y seguimos con el resto

(define (rellenar-con-ceros fila largo)
  (append fila  ; Añadimos ceros al final de la fila hasta que alcance el largo deseado
          (crear-ceros (- largo (length fila)))))  ; Crea los ceros necesarios y los añade

; FUSION Y PUNTAJE
(define (fusionar-y-puntuar fila)
  (cond
    [(null? fila) (list '() 0)]  ; Si la fila está vacía, retornamos una fila vacía y 0 puntos
    [(null? (cdr fila)) (list fila 0)]  ; Si solo queda un elemento, no se puede fusionar
    [(= (car fila) (cadr fila))  ; Si dos valores son iguales, se combinan
     (fusionar-y-puntuar-union
      (+ (car fila) (cadr fila))  ; La nueva baldosa será la suma de ambos valores
      (fusionar-y-puntuar (cddr fila)))]  ; Llamamos recursivamente para el resto de la fila
    [else
     (fusionar-y-puntuar-normal
      (car fila)
      (fusionar-y-puntuar (cdr fila)))]))  ; Si no se pueden fusionar, los dejamos tal como están

(define (fusionar-y-puntuar-union suma resultado-resto)
  (list
   (cons suma (resultado-tablero resultado-resto))  ; Insertamos la suma en la lista de resultados
   (+ suma (resultado-puntos resultado-resto))))  ; Sumamos los puntos obtenidos en esta fusión

(define (fusionar-y-puntuar-normal primero resultado-resto)
  (list
   (cons primero (resultado-tablero resultado-resto))  ; Dejamos el primer valor tal cual
   (resultado-puntos resultado-resto)))  ; Sumamos los puntos obtenidos de la siguiente parte de la fila

; MOVIMIENTO DE FILAS
(define (mover-fila-izquierda fila)
  (resultado-tablero (mover-fila-izquierda-resultado fila)))  ; Mueve la fila a la izquierda y obtiene el tablero actualizado

(define (mover-fila-izquierda-resultado fila)
  (mover-fila-izquierda-resultado-aux
   fila
   (fusionar-y-puntuar (quitar-ceros fila))))  ; Elimina los ceros y fusiona los valores

(define (mover-fila-izquierda-resultado-aux fila resultado)
  (list
   (rellenar-con-ceros
    (resultado-tablero resultado)
    (length fila))  ; Rellena la fila con ceros hasta el largo original
   (resultado-puntos resultado)))  ; Retorna la fila resultante y los puntos obtenidos

(define (mover-fila-derecha fila)
  (resultado-tablero (mover-fila-derecha-resultado fila)))  ; Mueve la fila a la derecha

(define (mover-fila-derecha-resultado fila)
  (mover-fila-derecha-resultado-aux
   (mover-fila-izquierda-resultado (invertir fila))))  ; Primero mueve a la izquierda después de invertir la fila

(define (mover-fila-derecha-resultado-aux resultado)
  (list
   (invertir (resultado-tablero resultado))  ; Invierte el resultado para devolverlo a su orden original
   (resultado-puntos resultado)))  ; Retorna los puntos acumulados

; MOVIMIENTO DE TABLEROS
(define (mover-izquierda tablero)
  (resultado-tablero (mover-izquierda-resultado tablero)))  ; Mueve todo el tablero a la izquierda

(define (mover-izquierda-resultado tablero)
  (cond
    [(null? tablero) (list '() 0)]  ; Si el tablero está vacío, no hay nada que mover
    [else
     (combinar-resultados-tablero
      (mover-fila-izquierda-resultado (car tablero))  ; Mueve la primera fila a la izquierda
      (mover-izquierda-resultado (cdr tablero)))]))  ; Llama recursivamente para el resto del tablero

(define (mover-derecha tablero)
  (resultado-tablero (mover-derecha-resultado tablero)))  ; Mueve todo el tablero a la derecha

(define (mover-derecha-resultado tablero)
  (cond
    [(null? tablero) (list '() 0)]  ; Si el tablero está vacío, no hay nada que mover
    [else
     (combinar-resultados-tablero
      (mover-fila-derecha-resultado (car tablero))  ; Mueve la primera fila a la derecha
      (mover-derecha-resultado (cdr tablero)))]))  ; Llama recursivamente para el resto del tablero

(define (combinar-resultados-tablero resultado-fila resultado-resto)
  (list
   (cons (resultado-tablero resultado-fila)  ; Combina las filas resultantes
         (resultado-tablero resultado-resto))
   (+ (resultado-puntos resultado-fila)  ; Suma los puntos de las filas
      (resultado-puntos resultado-resto))))  ; Suma los puntos de las filas combinadas

(define (primeros matriz)
  (cond
    [(null? matriz) '()]  ; Si la matriz está vacía, retornamos una lista vacía
    [else
     (cons (caar matriz)  ; Toma el primer elemento de la primera fila
           (primeros (cdr matriz)))]))  ; Recursivamente toma el primer elemento de cada fila

(define (restos matriz)
  (cond
    [(null? matriz) '()]  ; Si la matriz está vacía, retornamos una lista vacía
    [else
     (cons (cdar matriz)  ; Toma el resto de la primera fila
           (restos (cdr matriz)))]))  ; Recursivamente toma el resto de cada fila

(define (transponer matriz)
  (cond
    [(null? matriz) '()]  ; Si la matriz está vacía, retornamos una lista vacía
    [(null? (car matriz)) '()]  ; Si la primera fila está vacía, retornamos una lista vacía
    [else
     (cons (primeros matriz)  ; Toma las primeras columnas de cada fila
           (transponer (restos matriz)))]))  ; Transpone recursivamente el resto de la matriz

(define (transponer-resultado resultado)
  (list
   (transponer (resultado-tablero resultado))  ; Transpone el tablero
   (resultado-puntos resultado)))  ; Mantiene los puntos obtenidos

(define (mover-arriba tablero)
  (resultado-tablero (mover-arriba-resultado tablero)))  ; Mueve el tablero hacia arriba

(define (mover-arriba-resultado tablero)
  (transponer-resultado
   (mover-izquierda-resultado
    (transponer tablero))))  ; Transpone el tablero, mueve las filas hacia la izquierda, y vuelve a transponer

(define (mover-abajo tablero)
  (resultado-tablero (mover-abajo-resultado tablero)))  ; Mueve el tablero hacia abajo

(define (mover-abajo-resultado tablero)
  (transponer-resultado
   (mover-derecha-resultado
    (transponer tablero))))  ; Transpone el tablero, mueve las filas hacia la derecha, y vuelve a transponer

(define (mover-tablero-resultado tablero direccion)
  (cond
    [(eq? direccion 'left) (mover-izquierda-resultado tablero)]  ; Si la dirección es izquierda, mueve el tablero a la izquierda
    [(eq? direccion 'right) (mover-derecha-resultado tablero)]  ; Si la dirección es derecha, mueve el tablero a la derecha
    [(eq? direccion 'up) (mover-arriba-resultado tablero)]  ; Si la dirección es arriba, mueve el tablero hacia arriba
    [(eq? direccion 'down) (mover-abajo-resultado tablero)]  ; Si la dirección es abajo, mueve el tablero hacia abajo
    [else (list tablero 0)]))  ; Si no se reconoce la dirección, retornamos el tablero sin cambios

(define (mover-tablero tablero direccion)
  (resultado-tablero
   (mover-tablero-resultado tablero direccion)))  ; Obtiene el resultado del movimiento según la dirección

(define (mover-y-crear tablero direccion)
  (mover-y-crear-aux tablero (mover-tablero-resultado tablero direccion)))  ; Mueve el tablero y agrega una nueva baldosa aleatoria

(define (mover-y-crear-aux tablero resultado)
  (cond
    [(equal? (resultado-tablero resultado) tablero)  ; Si el tablero no cambió, retornamos el mismo tablero
     (list tablero 0)]
    [else
     (list
      (agregar-baldosa-aleatoria (resultado-tablero resultado))  ; Agrega una nueva baldosa aleatoria
      (resultado-puntos resultado))]))  ; Retorna los puntos obtenidos

; CONDICIONES DEL JUEGO
(define (fila-contiene? fila valor)
  (cond
    [(null? fila) #f]  ; Si la fila está vacía, no contiene el valor
    [(= (car fila) valor) #t]  ; Si encontramos el valor en la primera posición, retornamos #t
    [else (fila-contiene? (cdr fila) valor)]))  ; Si no, buscamos en el resto de la fila

(define (tablero-contiene? tablero valor)
  (cond
    [(null? tablero) #f]  ; Si el tablero está vacío, no contiene el valor
    [(fila-contiene? (car tablero) valor) #t]  ; Si la fila contiene el valor, retornamos #t
    [else (tablero-contiene? (cdr tablero) valor)]))  ; Si no, buscamos en el resto del tablero

(define (gano? tablero)
  (tablero-contiene? tablero 2048))  ; Si el tablero contiene una baldosa con valor 2048, el jugador gana

(define (hay-vacias? tablero)
  (not (null? (posiciones-vacias tablero))))  ; Verifica si hay posiciones vacías en el tablero

(define (adyacentes-iguales-en-fila? fila)
  (cond
    [(null? fila) #f]  ; Si la fila está vacía, no hay adyacentes iguales
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