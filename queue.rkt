#lang racket
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (make-queue) (mcons null null))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue) (mcar (front-ptr queue)))

(define (insert-queue queue item)
  (let ([new-pair (mcons item null)])
    (cond ((empty-queue? queue) 
           (set-mcar! queue new-pair)
           (set-mcdr! queue new-pair)
           queue)
          (else (set-mcdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue queue)
  (if (empty-queue? queue)
      (error "EMPTY QUEUE---DELETE-QUEUE" queue)
      (begin
        (set-front-ptr! queue (mcdr (front-ptr queue)))
        queue)))
          

(define a (make-queue))
