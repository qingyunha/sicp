#lang racket
(define (make-withdraw balance)
  (lambda (amount)
    (if(>= balance amount)
       (begin (set! balance (- balance amount))
              balance)
        "insuffient funds")))

(define withdraw
  (let ((balance 100))
    (lambda (x)
      (begin (set! balance (- balance x))
             balance))))

(define w1 (make-withdraw 100))

(define (make-accumulator base)
  (lambda (num)
    (begin (set! base (+ base num))
           base)))
(define acc (make-accumulator 0))


(define (make-monitored pro)
 (let ((count 0)) 
  (lambda (x)
    (cond ((eq? x "how-many-calls?")
             count)
            ((eq? x "reset count")
             (set! count 0))
            (else 
                  (set! count (+ count 1))
                  (print count)
                  (pro x))))))

(define s (make-monitored sqrt))

 (define (make-monitored1 proc)
  (let ((call-count 0))
    (define (dispatch m)
      (cond 
        ((eq? m 'how-many-calls?) call-count)
        ((eq? m 'reset-count) (set! call-count 0))
        (else
          (set! call-count (+ 1 call-count))
          (proc m))))
    dispatch))
 
 
 (define s1 (make-monitored1 sqrt))
 
 
 
 (define (make-count balance passwd)
   (define count 0)
   (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "insufficent funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (define (dispatch p z)
     (cond ((not (eq? p passwd))
             (print "passwd wrong")
             (set! count (+ count 1))
             (cond ((> count 7)
                callpolice
                )))
            (else (cond ((eq? z 'withdraw)
                         withdraw)
                        ((eq? z 'deposit))
                        (else
                         (error "unknown requst"  z))))))
   dispatch)
 
 (define callpolice "police coming")
 
 (define a (make-count 100 'abc))
 