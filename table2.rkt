(define (make-table)
  (list '*table))


(define (assocc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else
         (assocc key (cdr records)))))


(define (lookup key1 key2 table)
  (let ((subt (assocc key1 (cdr table))))
    (cond ((not subt) #f)
          (else
           (let ((record (assocc key2 (cdr subt))))
             (cond ((not record) #f)
                   (else (cdr record))))))))

(define (insert key1 key2 value table)
  (let ((subt (assocc key1 (cdr table))))
    (cond ((not subt)
           (set-cdr!  table (cons (list key1) (cdr table)))
           (set! subt (assocc key1 (cdr table)))
           (let ((record (assocc key2 (cdr subt))))
             (if record
                 (set-cdr! record value)
                 (set-cdr! subt (cons (cons key2 value) (cdr subt))))))
          (else
           (let ((record (assocc key2 (cdr subt))))
             (if record
                 (set-cdr! record value)
                 (set-cdr! subt (cons (cons key2 value) (cdr subt)))))))))
           
          
(define t (make-table))