#lang eopl

;===============================================================================
; PARTE 1: DEFINICIÓN DEL LENGUAJE Y SU GRAMÁTICA
;===============================================================================

(define-datatype valor-denotado valor-denotado?
  (numero-val (num number?))
  (texto-val (txt string?))
  (booleano-val (bool boolean?))
  (proc-val (proc procedure?)))

(define-datatype expresion expresion?
  (numero-lit (num number?))
  (texto-lit (txt string?))
  (var-exp (id symbol?))
  (primapp-bin-exp (exp1 expresion?) (prim-binaria symbol?) (exp2 expresion?))
  (primapp-un-exp (prim-unaria symbol?) (exp expresion?)))

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) (numero-val num))
      (texto-lit (txt) (texto-val txt))
      (var-exp (id) (buscar-variable id env))
      (primapp-bin-exp (exp1 prim exp2)
       (let ((val1 (evaluar-expresion exp1 env))
             (val2 (evaluar-expresion exp2 env)))
         (aplicar-primitiva-binaria prim val1 val2)))
      (primapp-un-exp (prim exp)
       (let ((val (evaluar-expresion exp env)))
         (aplicar-primitiva-unaria prim val))))))

; --- Aplicación de Primitivas ---
(define aplicar-primitiva-binaria
  (lambda (prim val1 val2)
    (cases valor-denotado val1
      (numero-val (n1)
        (cases valor-denotado val2
          (numero-val (n2)
            (cond
              [(eqv? prim '+) (numero-val (+ n1 n2))]
              [(eqv? prim '~) (numero-val (- n1 n2))]
              [(eqv? prim '/) (numero-val (/ n1 n2))]
              [(eqv? prim '*) (numero-val (* n1 n2))]
              [else (eopl:error 'aplicar-primitiva-binaria "Operador no válido para números: ~s" prim)]))
          (else (eopl:error 'aplicar-primitiva-binaria "El segundo operando no es un número: ~s" val2))))
      (texto-val (s1)
        (cases valor-denotado val2
          (texto-val (s2)
            (if (eqv? prim 'concat)
                (texto-val (string-append s1 s2))
                (eopl:error 'aplicar-primitiva-binaria "Operador no válido para textos: ~s" prim)))
          (else (eopl:error 'aplicar-primitiva-binaria "El segundo operando no es un texto: ~s" val2))))
      (else (eopl:error 'aplicar-primitiva-binaria "Tipo no soportado para operaciones binarias: ~s" val1)))))

(define aplicar-primitiva-unaria
  (lambda (prim val)
    (cases valor-denotado val
      (numero-val (n)
        (cond
          [(eqv? prim 'add1) (numero-val (+ n 1))]
          [(eqv? prim 'sub1) (numero-val (- n 1))]
          [else (eopl:error 'aplicar-primitiva-unaria "Operador no válido para números: ~s" prim)]))
      (texto-val (s)
        (if (eqv? prim 'longitud)
            (numero-val (string-length s))
            (eopl:error 'aplicar-primitiva-unaria "Operador no válido para textos: ~s" prim)))
      (else (eopl:error 'aplicar-primitiva-unaria "Tipo no soportado para operaciones unarias: ~s" val)))))

;===============================================================================
; PUNTO 2: GESTIÓN DEL AMBIENTE
;===============================================================================

(define ambiente-inicial
  (list (cons '@a (numero-val 1))
        (cons '@b (numero-val 2))
        (cons '@c (numero-val 3))
        (cons '@d (texto-val "hola"))
        (cons '@e (texto-val "FLP"))))

(define buscar-variable
  (lambda (id env)
    (if (null? env)
        (eopl:error 'buscar-variable "La variable no ha sido definida: ~s" id)
        (let ((primer-par (car env)))
          (if (eqv? (car primer-par) id)
              (cdr primer-par)
              (buscar-variable id (cdr env)))))))

;===============================================================================
; PUNTO 3: IMPLEMENTACIÓN DE LÓGICA BOOLEANA
;===============================================================================
(define valor-verdad?
  (lambda (valor)
    (cases valor-denotado valor
      (numero-val (num)
        (not (zero? num))) ; La función zero? es una forma limpia de escribir (= num 0)
      (else
       (eopl:error 'valor-verdad? "Se esperaba un valor numérico, pero se recibió: ~s" valor)))))

;===============================================================================
; PRUEBAS
;===============================================================================
(define run
  (lambda (exp)
    (evaluar-expresion exp ambiente-inicial)))

(eopl:printf "--- Pruebas de Búsqueda de Variables ---\n")
(eopl:printf "~s\n" (run (var-exp '@a)))
(eopl:printf "~s\n" (run (var-exp '@b)))
(eopl:printf "~s\n" (run (var-exp '@e)))

(eopl:printf "\n--- Pruebas de Operaciones ---\n")
(eopl:printf "~s\n" (run (primapp-bin-exp (var-exp '@a) '+ (var-exp '@b))))
(eopl:printf "~s\n" (run (primapp-bin-exp (var-exp '@d) 'concat (var-exp '@e))))
(eopl:printf "~s\n" (run (primapp-un-exp 'longitud (var-exp '@d))))
(eopl:printf "~s\n" (run (primapp-un-exp 'add1 (var-exp '@c))))

(eopl:printf "\n--- Pruebas de Booleanos (valor-verdad?) ---\n")
(eopl:printf "~s\n" (valor-verdad? (numero-val 5)))
(eopl:printf "~s\n" (valor-verdad? (numero-val 0)))
(eopl:printf "~s\n" (valor-verdad? (numero-val -1)))