#lang racket

(define (memo-proc pro)
  (let ([already-run? false] [result false])
    (lambda()
    (if (not already-run?)
        (begin
          (set! result (pro))
          (set! already-run? true)
          result)
        result))))

(define the-empty-s '())
(define s-null? null?)


(define-syntax-rule (s-cons a b) (cons a (d b)))
(define (s-car x) (car x))
(define (s-cdr x) (f (cdr x)))

(define-syntax-rule (d exp)  (memo-proc (lambda () exp)))

(define (f exp) (exp)) 

;;;流操作

(define (s-map pro . argment)
  (if (s-null? (car argment))
      the-empty-s
      (s-cons
       (apply pro (map s-car argment))
       (apply s-map (cons pro (map s-cdr argment))))))


(define (s-ref s n)
  (if (= n 0)
      (s-car s)
      (s-ref (s-cdr s) (- n 1))))

(define (s-for-each proc s)
  (if (s-null? s)
      'done
      (begin
        (proc (s-car s))
        (s-for-each proc (s-cdr s)))))

(define (s-enumerate-interval low high)
  (if (> low high)
      the-empty-s
      (s-cons low (s-enumerate-interval (+ low 1) high))))


(define (display-s s)
  (s-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (show x)
  (display-line x)
  x)






(define a (s-cons 1 
                  (s-cons 2
                          (s-cons 3 the-empty-s))))
(define b (s-cons 10
                  (s-cons 20
                          (s-cons 30 the-empty-s))))


(define (add . a)
  (apply + a))


