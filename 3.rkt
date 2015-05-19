#lang racket
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if(same-variable? exp var)
            1
            0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponent? exp)
         (let ((n (exponent exp)))
           (make-product (make-product n
                                       (make-exponent (base exp) (- n 1)))
                         (deriv (base exp) var))))
                       
        (else
         (error "unknown expression type ---- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        (else (list '+ a b))))
(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        (else (list '* a b))))
(define (make-exponent b e)
  (cond ((and (number? b) (number? e)) (** b e))
        ((=number? b 1) 1)
        ((=number? e 0) 1)
        (else (list '** b e))))
(define (** b e)
  (if(= e 0)
     1
     (* b (** b (- e 1)))))
     
        
(define (=number? a b)
  (and (number? a) (= a b)))


(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))
(define (product? x)
  (and (pair? x) (eq? '* (car x))))
(define (exponent? x)
  (and (pair? x) (eq? '** (car x))))

(define (addend x) (cadr x))
(define (augend x) (caddr x))

(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

 