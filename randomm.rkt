#lang racket
(define (random-in-range low high)
(let ((range (- high low)))
(+ low (random range))))

(define (mnote-carlo trials experiment)
(define (iter trials-remaining trials-passed)
(cond ((= trials-remaining 0)
(/ trials-passed trials))
((experiment)
(iter (- trials-remaining 1) (+ trials-passed 1)))
(else
(iter (- trials-remaining 1) trials-passed))))
(iter trials 0))


(define (estimate-integral-experiment x1 x2 y1 y2)
(define (in-circle x y)
(if (<= (+ (* (- x 5) (- x 5)) (* (- y 7) (- y 7))) 9)
#t
#f))
(lambda () (in-circle (random-in-range x1 x2) (random-in-range y1 y2))))

(define (estimate-integral P x1 x2 y1 y2 trials)
(/ (* (mnote-carlo trials (P x1 x2 y1 y2)) (* (abs (- x1 x2)) (* (abs (- y1 y2))))) 9))

(estimate-integral estimate-integral-experiment 2 8 4 10 10000)