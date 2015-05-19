#lang racket
(define a 0)
(define b 10)
(define n 10000)
(define h (/ (- b a) n))

(define (sum a b f)
  (foldl + 0
         (for/list ([i (in-range a b)])
           (f i))))

(define (Tn)
  (+ (* (/ h 2) (+ (f a) (f b)))
     (* h (sum 1 (- n 1) 
               (lambda (k) (f (+ a (* k h))))))))


(define (Sn)
  (+ (* (/ h 6) (f a) (f b))
     (* 2/3 h (foldl + 0
                     (for/list ([k (in-range 0 (- n 1))])
                       (f (+ a (* (+ 1/2 k) h))))))
     (* 1/3 h (foldl + 0
                     (for/list ([k (in-range 1 (- n 1))])
                       (f (+ a (* k h))))))))
  
  
(define (f x) x)