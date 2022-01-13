#lang racket
(require racket/trace)

;; Funcion principal: ejecuta esto para empezar
;; (Probablemente no hace falta modificar esta funcion)
(define(inicio )
  (cond [(empty? (imprimir " RESULTADO " (evaluar(validar-sintaxis(leer)))))null]
        [else inicio]))

;; Funcion  encargada de validar la sintaxis de la expresion
(define(validar-sintaxis expresion)
  (cond [(empty? expresion) null]
        [(not (validar-expresion expresion)) null]  
        [else expresion]))
              
;; Funcion encargada de validar una expresion lambda
;; <expresion> ::= <identificador>
;; <expresion> ::= (L   <identificador>  _  <expresion>)
;; <expresion> ::= (<expresion>    <expresion>)
(define (validar-expresion expresion)
  (validar-aux (sustituir-numeros (sustituir-alias expresion))))

(define (validar-aux expresion)
  (cond [(empty? expresion) #f]
        [(validar-identificador expresion) #t]                     ; Si la expresion es un identificador es VALIDO.
        [(and (list? expresion)(equal? 4 (length expresion))       ; Si es una lista que contiene 4 elementos
              (equal? 'L (car expresion))                          ; el primer elemento es "L"
              (validar-identificador (car (cdr expresion)))        ; su segundo elemento es un identificador
              (equal? '_ (car (cdr (cdr expresion))))              ; su tercer elemento es un "_"
              (validar-expresion (car (cdr (cdr (cdr expresion)))))) #t]  ; y su cuarto elemento una expresion es  VALIDO.
        [(and (list? expresion)(equal? 2 (length expresion))  ; si es una lista con dos elementos que son expresiones validas, es VALIDO.
              (and (validar-expresion (car expresion)) (validar-expresion (car (cdr expresion))))) #t]  
        [else #f]))

;; Funcion encargada de validar un identificador.
(define (validar-identificador identificador)  
  (cond [(and (not (list? identificador)) (not (string-contains? (symbol->string identificador) " "))) #t]
        [else #f]))
 
          
;; Funcion que dado un numero devuelve la formula de dado numero (solo positivos)
;; (ej: 1) por su formula (ej: (L f _ (L x _ (f x))) )
;;
(define(numero-a-formula num)
  (cond [(not (number? num)) null]   ;si es un numero retorna null
        [(< num 0) null]             ;si es menor a cero retorna null
        [else (append '(L f _)  (cons (append '(L x _) (cons (agregar-fs num) null)) null))]))  ;anhade los prefijos y los valores a la exprecion

;agrega el sufijo referente, va agregando dependiendo del valor numerico
(define (agregar-fs num)
  (cond [ (= 0 num) 'x ]          ; si el valor corresponde a 0 se agrega una x
        [ (= 1 num) (cons 'f (cons 'x null))]    ;cuando el valor numerico llegue a uno se agrega (f x) al final
        [else (cons 'f (cons (agregar-fs (- num 1)) null))])) ; se van agregando las f a medida que decrece el numero ingresado


;; Funcion encargada de convertir la formula a un numero
;; Ej:
;; (L f _ (L x _ (f x))) por el numero 1
;; o tambien
;; (L y _ (L p _ (y p))) por el numero 1
;;
;; (es la estructura que importa no el nombre de las variables!!)
;;
(define (formula-a-numero formula)
  (cond [(empty? formula) null]
        [(and (list? formula)(validar-expresion formula)(>= (length formula) 3)(list? (cadddr formula))(>= (length (cadddr formula)) 3)
              (equal? (cadr (cadddr formula))(cadddr (cadddr formula)))) '0]    ; Retorna 0 en caso de tener la estructura; (L f  (L x   x))
        [(and (list? formula)(validar-expresion formula)(>= (length formula) 3)(list? (cadddr formula))(>= (length (cadddr formula)) 3)
              (determinar-numero (cadddr (cadddr formula)) (cadr formula) (cadr (cadddr formula)) 0))]  ; Determina el posible numero en caso de
        [else formula]))                                                                           ; tener la estructura; (L f _ (L x _ ...)

(define (determinar-numero formula ident1 ident2 num)
  (cond [(and (equal? 2 (length formula))(equal? ident1 (car formula))(list? (cadr formula))) ; Si la lista tiene long 2, el segundo elemento es una lista y el primer elemento es igual
         (determinar-numero (cadr formula) ident1 ident2 (+ 1 num))]             ; al primer identificador (f en el ejemplo) llamara al inicio de la funcion aumentando en 1 num.
        [(and (equal? 2 (length formula))(equal? ident1 (car formula))(equal? ident2 (cadr formula))) (+ 1 num)] ; Si el segund o elemento ya no es una lista
        [else formula]))                                                        ; y es igual al segundo identificador (x en el ejemplo) retornara num aumentado en 1.


;; recorre la expresion y sustituye todos los numeros por formulas
(define (sustituir-numeros expresion)
  (cond [(empty? expresion) null]                                ; si es una lista vacia retorna null
        [(not (pair? expresion))                                 ; si no es un par/ es un solo atomo y pasa a la siguiente condicion
         (if (number? expresion) (numero-a-formula expresion) expresion)]  ;si es un numero, cambia el valor por su formula
        [else (cons (sustituir-numeros (car expresion))          ;pasa recursivamente sobre el car y el cdr de la lista 
                    (sustituir-numeros (cdr expresion)))]))

;; rerre la expresion y sustituye todas las formulas correspondiente a numeros con ellos 
(define (sustituir-formulas expresion)      
  (cond [(empty? expresion) null]                                        
        [(not (pair? expresion)) expresion]                      ; si no es una lista retorna la expresion
        [(and (pair? expresion)(number? (formula-a-numero expresion))) (formula-a-numero expresion)]  ;si es una lista analiza si es un numero y devuelve el valor que posee
        [else (cons (sustituir-formulas (car expresion))          ;itera recursivamente sobre la lista
                    (sustituir-formulas (cdr expresion)))]))

;; Funcion encargada de la reduccion-beta.
;; Ej: (L x _ (x x)) 2 ==> (2 2)
;; realiza 1 sola reduccion
(define (reduccion-beta expresion)
  (cond [(empty? expresion) null]
        [(not (reducible? expresion)) expresion]    ; Si la expresion no es reducible, retorna la expresion.
        [(and (equal? 2 (length expresion)) (reducible? (car expresion))) ; Si es una lista de long 2, y el primer elemento es una lista reducible
         (reducir expresion (car expresion))]                             ; lo reducira completamente.
        [(and (equal? 2 (length expresion)) (reducible? (cadr expresion))); Si es una lista de long2, y el segundo elemento es una lista reducible
         (reducir expresion (cadr expresion))]                            ; lo reducira completamente.
        [else (reducir expresion expresion)]))      ; Si ambos elementos estan reducidos, los reducira entre si.      

; Funcion que realiza la reduccion.
(define (reducir expresion expresion-a-reducir)
  (reemplazar expresion (buscar-reducible expresion-a-reducir)
              (reemplazar (car (cdddar (buscar-reducible expresion-a-reducir)))
                          (cadar (buscar-reducible expresion-a-reducir))
                          (cadr (buscar-reducible expresion-a-reducir)))))


;; Funcion encargada de determinar si una expresion es reducible.
(define (reducible? expresion)
  (cond [(empty? expresion) #f]
        [(not (list? expresion)) #f] ; Si no es una lista es que es solo un identificador
        [(and (validar-expresion expresion) (equal? 2 (length expresion)) (list? (car expresion)) ; Si es una expresion y tiene 2 elementos que tambien son expreciones,
              (if (list? (caar expresion)) (equal? 'L (caaar expresion)) (equal? 'L (caar expresion)))) #t] ; y el primer elemento esta de la forma (L x _ (expresion)), es reducible.
        [(and (validar-expresion expresion) (equal? 4 (length expresion))) (reducible? (cadddr expresion))] 
        [(and (validar-expresion expresion) (equal? 2 (length expresion)) (list? (cadr expresion)) (reducible? (cadr expresion)))  #t] 
        [else #f]))

;; Funcion encargada de retornar la expresion reducible mas profunda de la lista.
(define (buscar-reducible expresion)
  (cond [(not (reducible? expresion)) expresion] ; Si ya no es reducible retorna la expresion.
        [(and (equal? 2 (length expresion))(reducible? (car expresion))) (buscar-reducible (car expresion))]  
        [(and (equal? 2 (length expresion))(reducible? (cadr expresion))) (buscar-reducible (cadr expresion))]
        [(equal? 2 (length expresion)) expresion]
        [else (buscar-reducible (cadddr expresion))]))

;; Funcion encargada de reemplazar un primer valor por un segundo dentro de una lista.
(define (reemplazar lista exp new-exp)
  (cond [(empty? lista) null]
        [(equal? lista exp) new-exp]
        [(not (pair? lista)) (if (equal? lista exp) new-exp lista)]
        [else (cons (reemplazar (car lista) exp new-exp)(reemplazar (cdr lista) exp new-exp))]
        ))


;esta funcion sustituye las expresiones por su denominacion lambda correspondiente 
(define (sustituir-alias expresion)
  (cond [(empty? expresion) null]                        ; si es una lista vacia 
        [(not (pair? expresion))                         ; si no es un par/ es un solo atomo
         (cond [(equal? 'SUMA expresion) '(L m _ (L n _ ( L f _ ( L x _ ((m f) ( (n f) x))))))]
               [(equal? 'MULT expresion) '(L m _ (L n _ ( L f _ (m (n f)))))]
               [(equal? 'PRED expresion) '(L n _ (L f _ (L x _ ((n (L g _ (L h _ (h (g f)))))((L u _ x)( L u _ u))))))]
               [(equal? 'TRUE expresion) '(L x _ (L y _ x))]
               [(equal? 'SUCC expresion) '(L n _ (L f _ ( L x _ (f ((n f) x)))))]
               [(equal? 'FALSE expresion) '(L x _ (L y _ y))]
               [(equal? 'OR expresion) '(L p _ (L q _ (p (p q))))]
               [else expresion])]                         ;si es un numero, cambia el valor por su formula
        [else (cons (sustituir-alias  (car expresion))    ;pasa recursivamente sobre el car y el cdr de la lista 
                    (sustituir-alias  (cdr expresion)))]))



;; Funcion encargada de evaluar la expresion y obtener un resultado
;; combina todas las funciones anteriores para sustituir, luego hacer reducciones
;; hasta obtener un resultado simplificado.. y finalmente sustituye formulas por
;; numeros.
;;
(define (evaluar expresion)
  (print expresion) (newline)              ; Imprime la expresion.
  (imprimir "[sust - num] -------->" (sustituir-numeros expresion)) ; Imprime la expresion con los numeros sustituidos.
  (imprimir "[sust - alias] ------>" (sustituir-alias (sustituir-numeros expresion))) ; Imprime la expresion con los alias sustituidos.
  (evaluar-aux-aux (evaluar-aux (sustituir-alias (sustituir-numeros expresion)))))    ; Evalua la expresion e imprime el resultado.

(define (evaluar-aux expresion)
  (imprimir "[Reduccion beta] -------->  " (reduccion-beta expresion))                ; Realiza la reduccion-beta imprimiendo cada paso.
  (cond [(empty? expresion) null]
        [(reducible? expresion) (evaluar-aux (reduccion-beta expresion))]             ; Si aun es reducible llamara al inicio de la funcion-aux.
        [else expresion]))

(define (evaluar-aux-aux expresion)                           
  (if (equal? expresion (sustituir-formulas expresion))                               ; Sustituye las formulas por los numeros si es que es necesario.
      (newline) (imprimir "[sust - form] ----->" (sustituir-formulas expresion)))
  (sustituir-formulas expresion))



;;Funcion encargada de imprimir la expresion
(define (imprimir expresion nombre)                                                   
  (print expresion)
  (print nombre)
  (newline))

;;funcion encargada de obtener datos del teclado
(define (leer)
  (read))

;;Funciones de depuracion
;;Habilite y deshabilite segun su necesidad
;(trace inicio)
;;(trace validar)
;(trace evaluar)
;(trace imprimir)
;(trace leer)

;---------------------------------------------------------------------------------------------------------------------------------------------------


(define(inicio-alternativo)
  (cond [(empty? (imprimir " RESULTADO " (evaluar(validar-sintaxis(sintaxis-alternativa (leer))))))null]
        [else inicio]))



(define (sintaxis-alternativa expresion)
  (cond [(empty? expresion) null]
        [else (agrup-aux (reduce list (car (agrupador-de-terminos expresion)) (cdr (agrupador-de-terminos expresion))))]))


;agrupa si el valor es agrupable
(define (agrupador expresion)
  (cond [(empty? expresion) null]
        [(not (agrupable? (append '(L) expresion ))) expresion]
        [else (reemplazar expresion (cdr expresion) (cons '_ (cons (append '(L) (cdr expresion)) null)))]))
         
; reemplaza la expresion compreta de definciones de funci[on
(define (agrup-aux expresion)
  (cond [(empty? expresion)]
        [(agrupable? expresion) (agrup-aux (reemplazar expresion (buscar-agrupable expresion) (agrupador (buscar-agrupable expresion))))]
        [else expresion]))


;si las definiciones son agrupables
(define (agrupable? expresion)
  (cond [(empty? expresion) #f]
        [(not (list? expresion)) #f]
        [(and (equal? 2 (length expresion)) (list? (car expresion))
              (if   (list? (caar expresion)) (and (equal? 'L (caar expresion)) (not (equal? '_ (car (cddaar expresion)))) (validar-identificador (car ( cddaar expresion))))
                    (and (equal? 'L (caar expresion)) (not (equal? '_ (caddar expresion))) (validar-identificador (caddar expresion))) )) #t]                                                                         
        [(and (equal? 2 (length expresion))  (list? (car expresion))  (agrupable? (cadr expresion))) #t]
        [(and (equal? 2 (length expresion))  (list? (cadr expresion))  (agrupable? (cadr expresion))) #t]
        [(equal? 4 (length expresion)) (agrupable? (cadddr expresion))]
        [(and (< 4 (length expresion)) (and (list? expresion) (equal? 'L (car expresion)) (validar-identificador (car (cdr expresion)))
                                            (not (equal? '_ (car (cdr (cdr expresion))))) (validar-identificador (car (cdr (cdr expresion)))))) #t]
       
        [else #f]))


;devuelve las definiciones agrupadas 
(define (buscar-agrupable expresion)
  (cond [(not (agrupable? expresion)) expresion]
        [(and (equal? 2 (length expresion)) (agrupable? (car expresion))) (buscar-agrupable (car expresion))]
        [(and (equal? 2 (length expresion)) (agrupable? (cadr expresion))) (buscar-agrupable (cadr expresion))]
        [(equal? 2 (length expresion)) expresion]
        [(equal? 4 (length expresion))  (buscar-agrupable (cadddr expresion))]
        [else  (buscar-agrupable (cdr expresion))] 
        ))
         

;agrega los valores por derecha
(define (reduce fn init list)
  (if (null? list) init
      (reduce fn (fn init (car list)) (cdr list))))

;(trace agrupador)
                                ;-----------------------------------------------------------------------

;agrupa el cuerpo de la funcion
(define (agrupador-de-terminos expresion)
  (cond [(empty? expresion) null]
        [(pair? (car expresion)) (cons  (agrupador-de-terminos (car expresion)) (agrupador-de-terminos (cdr expresion)))]
        [(equal? (car expresion) '_) (append '(_) (agrupador-de-terminos (cdr (cons ' _ (cons  (reemplazar  (cdr expresion)  (cdr expresion) (agp-ter-aux (cdr expresion))) null))))) ]
        [else (cons (car expresion) (agrupador-de-terminos  (cdr expresion)))]))


;agrupa todas las expresiones
(define (agp-ter-aux expresion)
  (cond [(empty? expresion) null]                                                                                                                    ;si esta vacio retorna null
        [(and (=  1 (length   expresion)) (validar-identificador (car  expresion)))   (car expresion) ]                                              ;si es de long 1 y un identificador, retorna su identificador
        [(and (=  1 (length   expresion)) (list?   expresion))  (agp-ter-aux (car expresion))]                                                       ;si es de long 1 y una lista, agrupa su lista interna                 
        [(and (list? (car expresion)) (not (equal? (caar expresion) 'L))) (cons (agp-ter-aux (car expresion)) (agp-ter-aux (cdr expresion)))]
        [(>= 2 (length expresion)) ( cond
                                      [(and (list? (car expresion)) (not (equal? (caar expresion) 'L))) (cons (agp-ter-aux (car expresion)) (agp-ter-aux   (cdr expresion)))]
                                      [(and (list? (cadr expresion)) (not (equal? (caadr expresion) 'L))) (cons (car expresion) (cons (agp-ter-aux (cdr expresion)) null))]
                                      [else expresion])]
        
        [else (cons (car expresion)  (cons (agp-ter-aux (cdr expresion)) null))]))


 
;define condiciones para que sea agrupable
(define (agrupable-term? expresion)
  (cond [(empty? expresion) #f]      ;si está vacío no es agrupable.
        [(and (equal? (car expresion) '_) (not (and (=  1 (length (cdr expresion))) (validar-identificador (cdr expresion))))) #t] ;agrupable si esta despues de un guion bajo y no es un solo verificador  ni de long. 1
        [else #f]))                  ;sino, falso.