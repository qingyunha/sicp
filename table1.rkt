(define (make-table)
  (list '*table))

(define (lookup key table)
  (let ((record (assocc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assocc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else
         (assocc key (cdr records)))))

(define (insert key value table)
  (let ((record (assocc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))
        
(define t (make-table))
(insert 'a 1 t)