#lang racket
;;;队列
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (make-queue) (mcons null null))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue) (mcar (front-ptr queue)))

(define (insert-queue! queue item)
  (let ([new-pair (mcons item null)])
    (cond ((empty-queue? queue) 
           (set-mcar! queue new-pair)
           (set-mcdr! queue new-pair)
           queue)
          (else (set-mcdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "EMPTY QUEUE---DELETE-QUEUE" queue)
      (begin
        (set-front-ptr! queue (mcdr (front-ptr queue)))
        queue)))
          




 
;;;
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not signal)
  (if (= signal 0)
      1
      0))


(define (and-gate a1 a2 output)
  (define (add-action)
    (let ((new-value (logical-add (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay 
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 add-action)
  (add-action! a2 add-action)
  'ok)



(define (logical-add s1 s2)
  (if (not (and (= s1 1) (= s2 1)))
      0
      1))

(define (or-gate a1 a2 output)
  (define (or-action)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay 
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)

(define (logical-or s1 s2)
  (if (and (= s1 0) (= s2 0))
      0
      1))


;;;半加器
(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))



(define (make-wire)
  (let ([signal-value 0] [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (clear)
      (set! signal-value 0)
      (set! action-procedures '()))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'show-action) (display action-procedures))
            ((eq? m 'clear) (clear))
            (else (error "Unkonwn operation--WIRE" m))))
  dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))

(define (set-signal! wire value) 
  ((wire 'set-signal!) value))
    
(define (add-action! wire action) 
  ((wire 'add-action!) action))
  
(define (clear wire) (wire 'clear))  
(define (show-action wire) (wire 'show-action))   
;;待处理的表

  
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
  
  
(define (make-time-segment time queue)
   (cons time queue))

  

  (define (segment-time s) (car s))
  (define (segment-queue s) (cdr s))
  
  
  (define (make-agenda) (mcons 0 null))
  (define the-agenda (make-agenda))
  
  (define (current-time agenda) (mcar agenda))
  (define (set-current-time! agenda time)
    (set-mcar! agenda time))
  
  (define (segments agenda) (mcdr agenda))
  (define (set-segments! agenda segments) (set-mcdr! agenda segments))
  
  (define (first-segment agenda) (mcar (segments agenda)))
  (define (rest-segments agenda) (mcdr (segments agenda)))
  
  (define (empty-agenda? agenda) (null? (segments agenda)))
  
  
  (define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
      (or (null? segments)
          (< time (segment-time (mcar segments)))))
    
    (define (make-new-time-segment time action)
      (let ([q (make-queue)])
        (insert-queue! q action)
        (make-time-segment time q)))
    
    (define (add-to-segments! segments)
      (if (= (segment-time (mcar segments)) time)
          (insert-queue! (segment-queue (mcar segments)) action)
          (let ([rest (mcdr segments)])
            (if (belongs-before? rest)
                (set-mcdr! segments
                           (mcons (make-new-time-segment time action) (mcdr segments) ))
                (add-to-segments! rest)))))
    
    (let ([segments (segments agenda)])
      (if (belongs-before? segments)
          (set-segments!
           agenda
           (mcons (make-new-time-segment time action) segments))
          (add-to-segments! segments))))
  
  
  (define (remove-first-agenda-item! agenda)
    (let ([q (segment-queue (first-segment agenda))])
      (delete-queue! q)
      (if (empty-queue? q)
          (set-segments! agenda (rest-segments agenda))
          null)))
  
 (define (first-agenda-item agenda)    
   (if (empty-agenda? agenda)
       (error "Agenda is empty--FIRST-AGNEDA-ITEM")
       (let ((first-seg (first-segment agenda)))
         (set-current-time! agenda (segment-time first-seg))
         (front-queue (segment-queue first-seg)))))
  
  
  
  ;;; 驱动
  (define i 0)
(define (propagate)
  (if (empty-agenda? the-agenda)
      (display i)
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (set! i (+ i 1))
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;;;监视器
(define (probe name wire)
  (add-action! wire 
               (lambda ()
                 (newline)
                 (display name)
                 (display "  ")
                 (display (current-time the-agenda))
                 (display "  new-value= ")
                 (display (get-signal wire))
                 (newline)
                 (newline))))




(define l1 (make-wire))
(define l2 (make-wire))
(define l3 (make-wire))
(define l4 (make-wire))
(define l5 (make-wire))
(define l6 (make-wire))
(inverter l1 l2)
(and-gate l3 l4 l5)
(or-gate l3 l4 l6)
(probe 'and-output l5)
(probe 'or-output l6)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define carry (make-wire))
(define sum (make-wire))

(probe 'sum sum)
;(probe 'carry carry)

(half-adder input-1 input-2 sum carry)