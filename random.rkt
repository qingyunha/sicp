#lang racket
(define (rand-update x)
  (remainder (+ (* x 29) 37) 1000))
(define random-init 0)
(define rand
  (let ((x random-init))
    (lambda()
      (set! x (rand-update x))
      x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random 1000) (random 1000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remainding trials-passed)
    (cond  ((= trials-remainding 0)
            (/ trials-passed trials))
           ((experiment)
            (iter (- trials-remainding 1) (+ trials-passed 1)))
           (else 
            (iter (- trials-remainding 1) trials-passed))))
  (iter trials 0))
        


(define (estimate-pii trials)
  (sqrt (/ 6 (test trials))))
(define (test trials)
  (define (iter remainding passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= remainding 0)
               (/ passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- remainding 1) (+ passed 1) x2))
              (else
               (iter (- remainding 1) passed x2))))))
  (iter trials 0 0))
          

(define (estimate-integral p x1 y1 x2 y2 trials)
  (* (* (abs (- x1 x2)) (abs (- y1 y2)))
     (monte-carlo trials p)))
(define (p) 
  (< (+ (sqr (rand)) (sqr (rand))) 1))

(define (r)
  (define (iter x)
    (cond ((> x 0)
           (display (rand))
           (display " ")
           (iter (- x 1)))
          (else
           null)))
  (iter 100))