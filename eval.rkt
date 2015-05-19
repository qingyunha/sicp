;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eval) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")))))
; A BSL-expr is one of: 
; – Number
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)
; - (cons Symbol [list-of-BSL-fun-expr])

(define-struct add [left right])
(define-struct mul [left right])

;BSL-value is a number

; An AL (association list) is [List-of Association].
; An Association is (cons Symbol (cons Number empty)).

;An DL (definition list) is [list-of Definition]
;An Definition is def
(define-struct def [name para body])

;BSL-da-all is  [list-of Association or DL]



;A BSL-fun-def is a
;(make-def Symbol Symbol BSL-fun-expr)
;(define (f x) (+ 3 x))
;(define (g y) (f (* 2 y)))
;(define (h v) (+ (f v) (g v)))
(define ff (make-def 'ff 'x (make-add 3 'x)))
(define g (make-def 'g 'y (cons 'ff (cons (make-mul 2 'y) empty))))
(define h (make-def 'h 'v (make-add (cons 'ff (cons 'v empty)) (cons 'g (cons 'v empty)))))

;BSL-fun-def* is a DL

(define da-fgh (list ff g h))

(define AL '((x 5) (y 3)))


(define all-da (append AL da-fgh))


(define e1 (make-add (make-mul 3 3)
          (make-mul 4 4)))

(define e2 (make-add 'x 3))
(define e3 (make-add (make-mul 'x 'x)
          (make-mul 'y 'y)))

(define b (make-add 'x 1))
(define f (cons 'f (cons 8 empty)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVAL

(define (eval-expression bsl-exp)
  (cond [(add? bsl-exp)
         (+ (eval-expression (add-left bsl-exp)) 
            (eval-expression (add-right bsl-exp)))]
        [(mul? bsl-exp)
         (* (eval-expression (mul-left bsl-exp)) 
            (eval-expression (mul-right bsl-exp)))]
        [else bsl-exp]))

(define (eval-variable e)
  (if (numeric? e) 
      (eval-expression e)
      (error "impossible to evaluate an expression that contains a variable.")))

;BSL-var-exp AL -> BSL-value 
;(check-expect (eval-variable* e3 AL) 34)
(define (eval-variable* e da)
  (local(
         (define ee 
              (foldl (lambda (assoc base)(subst base (car assoc) (cadr assoc))) e da)))
    (eval-variable ee)))

; BSL-var-expr AL -> Number
;(check-expect (eval-var-lookup e3 AL) 34)
(define (eval-var-lookup e da)
  (cond [(add? e)
         (+ (eval-var-lookup (add-left e) da) 
            (eval-var-lookup (add-right e) da))]
        [(mul? e)
         (* (eval-var-lookup (mul-left e) da) 
            (eval-var-lookup (mul-right e) da))]
        [(number? e) e]
        [else (lookup-con da e)]))

  
;AL symbol -> BSL-value
;(check-expect (lookup-con AL 'x) 5)
;(check-expect (lookup-con AL 'y) 3)
(define (lookup-con da x)
  (cond [(empty? da) (error "not find")]
        [else (if (symbol=? x (caar da))
                  (cadar da)
                  (lookup-con (cdr da) x))]))
  

(define (eval-definition1 e f x b) 
  (cond [(add? e)
         (+ (eval-definition1 (add-left e) f x b) 
            (eval-definition1 (add-right e) f x b))]
        [(mul? e)
         (* (eval-definition1 (mul-left e) f x b) 
            (eval-definition1 (mul-right e) f x b))]
        [(number? e) e]
        [(symbol? e) (lookup-con empty e)]
        [else (eval-definition1 (subst b x (eval-definition1 (cadr e) f x b)) f x b)]))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da 
; or signal "undefined function" if da does not contain one
;(check-expect (lookup-def da-fgh 'g) g)
(define (lookup-def da f)
  (cond [(empty? da) (error "not find fuction")]
        [else (if (symbol=? f (def-name (car da)))
                  (car da)
                  (lookup-def (cdr da) f))]))

;BSL-fun-exp BSL-fun-def* -> BSL-value
;(check-expect (eval-function* (cons 'ff (cons 0 empty)) da-fgh) 3)
;(check-expect (eval-function* (cons 'g (cons 0 empty)) da-fgh) 3)
(define (eval-function* e da)
  (cond [(add? e)
         (+ (eval-function* (add-left e) da) 
            (eval-function* (add-right e) da))]
        [(mul? e)
         (* (eval-function* (mul-left e) da) 
            (eval-function* (mul-right e) da))]
        [(number? e) e]
        [(symbol? e) (lookup-con empty e)]
        [else 
         (local (
                 (define f (lookup-def da (car e)))
                 (define arg (eval-function* (cadr e) da)))
           (eval-function* (subst (def-body f) (def-para f) arg) da))]))
         
;BSL-da-all -> number
(define (lookup-con-def da x)
  (cond [(empty? da) (error "constant not find")]
        [else (if (and (not (def? (car da))) (symbol=? x (caar da)))
                  (cadar da)
                  (lookup-con-def (cdr da) x))]))
                  

(define (lookup-fun-def da x)
  (cond [(empty? da) (error "function not find")]
        [else (if (and (def? (car da)) (symbol=? x (def-name (car da))))
                  (car da)
                  (lookup-fun-def (cdr da) x))]))

;(check-expect (eval-all 'x all-da) 5)
;(check-expect (eval-all '(ff 0) all-da) 3)
;(check-expect (eval-all '(ff x) all-da) 8)
(define (eval-all e da)
  (cond [(add? e)
         (+ (eval-all (add-left e) da) 
            (eval-all (add-right e) da))]
        [(mul? e)
         (* (eval-all (mul-left e) da) 
            (eval-all (mul-right e) da))]
        [(number? e) e]
        [(symbol? e) (lookup-con-def da e)]
        [else 
         (local (
                 (define f (lookup-fun-def da (car e)))
                 (define arg (eval-all (cadr e) da)))
           (eval-all (subst (def-body f) (def-para f) arg) da))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BSL-var-exp symbol number -> BSL-var-exp
;(check-expect (subst e2 'x 2) (make-add 2 3))

(define (subst e x v)
  (cond [(add? e) (make-add (subst (add-left e) x v) (subst (add-right e) x v))]
        [(mul? e) (make-mul (subst (mul-left e) x v) (subst (mul-right e) x v))]
        [(number? e) e]
        [(symbol? e) (if (symbol=? e x) v e)]
        [else (cons (car e) (cons (subst (cadr e) x v) empty))]))


;BSL-var-exp -> boolen
;(check-expect (numeric? e1) true)
;(check-expect (numeric? e2) false)

(define (numeric? e)
  (cond [(add? e)
         (and (numeric? (add-left e)) (numeric? (add-right e)))]
        [(mul? e)
         (and (numeric? (mul-left e)) (numeric? (mul-right e)))]
        [(number? e) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PARSE
(define WRONG "wrong kind of S-expression")
(define (atom? s) (or (symbol? s) (number? s) (string? s)))
; S-expr -> BSL-expr
; creates representation of a BSL expression for s (if possible)
(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
 
          ; SL -> BSL-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [(and (= L 2) (symbol? (first s))) (cons (first s) (cons (parse (cadr s)) empty))]
                [else
                 (error WRONG)])))
 
          ; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(symbol? s) s]
              [else (error "not allowed")])))
    (parse s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define WRONGs "wrong kind of S-expression")
 
 
;{S-expr} -> (tech "BSL-fun-def")
; creates representation of a BSL definition for s (if possible)
(define (def-parse s)
  (local (; S-expr -> BSL-fun-def
          (define (def-parse s)
            (cond
              [(atom? s) (error WRONG)]
              [else
               (if (and (= (length s) 3) (eq? (first s) 'define))
                   (head-parse (second s) (parse (third s)))
                   (error WRONG))]))
          ; S-expr BSL-expr -> BSL-fun-def
          (define (head-parse s body)
            (cond
              [(atom? s) (list s body)]
              [else
               (if (not (= (length s) 2))
                   (error WRONG)
                   (local ((define name (first s))
                           (define para (second s)))
                     (if (and (symbol? name) (symbol? para))
                         (make-def name para body)
                         (error WRONG))))])))
    (def-parse s)))



    
(define (da-parse sl)
  (foldl (lambda (s base) (cons (def-parse s) base)) empty sl))

(define (eval-all-sexpr s sl)
  (local (
          (define e (parse s))
          (define da (da-parse sl)))
    (eval-all e da)))

