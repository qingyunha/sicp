#lang racket
(define (square x) (* x x))
;complex

(define (add-comaplex x y)
  (make-from-real-imag (+ (real-part x) (real-part y))
                       (+ (imag-part x) (imag-part y))))
(define (sub-complex x y)
  (make-from-real-imag (- (real-part x) (real-part y))
                       (- (imag-part x) (imag-part y))))

(define (mul-complex x y)
  (make-from-mag-ang (* (magnitude x) (magnitude y))
                     (+ (angle x) (angle y))))
(define (div-complex x y)
  (make-from-mag-ang (/ (magnitude x) (magnitude y))
                     (- (angle x) (angle y))))

;rect angle
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (make-from-real-imag x y) (cons x y))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
