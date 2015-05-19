#lang racket
(define (sq x)
(define (sq-it guss )
  (if (goodenough? guss )
       guss
       (sq-it (improve guss ))))
  
 (define (improve guss )
   (/  (+ guss (/ x guss)) 2))
  
  (define (goodenough? guss )
     (< (abs (- (* guss guss ) x)) 0.001))
     (sq-it 1.0)
  )

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (sum-it term a next b) 
  (define (sum-iter a rezult)
    (if (> a b)
        rezult
        (sum-iter (next a) (+ rezult (term a)))))
  (sum-iter a 0))
(define (accumulate combiner null-value term a next b) 
    (if (> a b) null-value 
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))
  

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (identity x) x)
(define (inc n) (+ n 1)) 
(define (cub x) (* x x x))   
(define (simpson-integral f a b n) 
   (define h (/ (- b a) n)) 
   (define (yk k) (f (+ a (* h k)))) 
   (define (simpson-term k) 
     (* (cond ((or (= k 0) (= k n)) 1) 
              ((odd? k) 4.0) 
              (else 2.0)) 
        (yk k))) 
   (* (/ h 3.0) (sum simpson-term 0 inc n)))           
          
(define (average a b)
  (/ (+ a b) 2.0))

(define (search f n p)
  (let ((mid (average n p)))
    (if (close-enough? n p)
        mid
        (let ((test (f mid)))
          (cond ((positive? test) (search f n mid))
                ((negative? test) (search f mid p))
                (else mid))))))
(define (close-enough? x y)
  (< (abs(- x y)) 0.00001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "values are not of opposite sign" a b)))))
;(define (fix-point f guss)
 ; (let ((next (f guss)))
  ;  (if (close-enough? guss next)
   ;     next
    ;    (fix-point f next))))


(define (fix-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
 ;     (display next)
  ;    (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sq1 x)
    (fix-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))




(define (div f) 
  (define dx 0.00001)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((div f) x)))))

(define (newton-method f guess)
  (fix-point (newton-transform f) guess))

(define (sq2 x)
  (newton-method (lambda (y) (- (* y y) x)) 1.0)) 


(define one2four '(1 2 3 4))
(define (list-ref list n)
  (if(= n 0)
     (car list)
     (list-ref (cdr list) (- n 1))))

(define (list-length list)
  (if(null? list)
     0
     (+ (list-length (cdr list)) 1)))

(define (append list1 list2)
  (if(null? list1)
     list2
     (cons (car list1) (append(cdr list1) list2))))

(define (delete-tail list)
  (if(null? (cdr list))
     null
     (cons (car list) (delete-tail (cdr list)))))

(define (reverse list)
  (if(null? list)
     null
     (cons (list-ref list (- (length list) 1)) (reverse (delete-tail list)))))


  
 (define (reverse1 items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) (cons (car items) result)))) 
  (iter items null))
 
(define (reverse-deep items)
  (define (iter item result)
    (cond ((null? item) result)
          (else (iter (cdr item) 
                      (if (pair? (car item))
                          (cons (reverse-deep (car item)) result)
                          (cons (car item) result))))))
  (iter items '()))
 
(define (separate-even list)
   (define (iter p l)
     (if(null? l)
        p
        (if(odd? (car l))
          (iter p (cdr l))
          (iter (cons (car l) p) (cdr l)))))
   (iter '() list))
 (define (separate-odd list)
   (define (iter p l)
     (if(null? l)
        p
        (if(not (odd? (car l)))
          (iter p (cdr l))
          (iter (cons (car l) p) (cdr l)))))
   (iter '() list))
  
 (define (same-parity x . z)
   (if(odd? x)
      (cons x (reverse(separate-odd z)))
      (cons x (reverse(separate-even z)))))
 
 (define (map proc items)
   (if(null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))
 
 ;;the difference cond and if!!!!!
  (define (for-each proc items) 
   (cond ((not (null? items)) 
          (proc (car items)) 
          (for-each proc (cdr items)))))
 
; (define (for-each proc items)
 ;  (if(null? items)
  ;    null
   ;   (proc (car items)))
    ;  (for-each proc (cdr items)))

 
 (define (count-leaves x)
   (cond ((null? x) 0)
         ((not (pair? x)) 1)
         (else (+ (count-leaves (car x))
                  (count-leaves (cdr x))))))
 ;wrong
 (define (fringe5 item)
   (cond ((null? item) null)
         ((not (pair? item)) (list item))
         (else (append (fringe5 (car item)) (fringe5 (cdr item))))))
 
 
 
 
 (define (fringe items)
   (define (iter item )
     (if(null? item)
        null
        (cons (if(pair? (car item))
                 (iter (car item))
                 (car item))
              (iter (cdr item)))))
   (iter items))
         ;wrong!
   (define (fringe1 tree) 
   (define nil '()) 
   (cond ((null? tree) nil) 
         ((not (pair? tree)) (list tree)) 
         (else (append (fringe (car tree)) (fringe (cdr tree)))))) 
   
   
   ;right
    (define (fringe2 tree) 
   (define nil '()) 
  
   (define (build-fringe x result) 
     (cond ((null? x) result) 
           ((not (pair? x)) (cons x result)) 
           (else (build-fringe (car x)  
                               (build-fringe (cdr x) result)))))
      (build-fringe tree nil))
   
   
   (define (fringe3 tree) 
   (define nil '()) 
   (if (null? tree)  
       nil 
       (let ((first (car tree))) 
         (if (not (pair? first)) 
             (cons first (fringe (cdr tree))) 
             (append (fringe first) (fringe (cdr tree))))))) 
 
 (define (pascal x y)
   (if(or (= x y) (= y 1))
      1
      (+ (pascal (- x 1) (- y 1))
         (pascal (- x 1) y))))

 (define (print-pascal n)
   (define (iter-y x count)
     (cond ((< count x)
             (display (pascal x (+ 1 count)))
             (display " ")
             (cond ((= x (+ count 1))
                    (newline)))
             (iter-y x (+ count 1)))))
   
   
   (define (iter-x c)
     (cond ((< c n)
            (iter-y (+ c 1) 0)
            (iter-x (+ c 1)))))
   (iter-x 0))
   
 
 (define (f n)
   (cond ((< n 3) (identity n))
         (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
 (define (ft n)
   (define (iter a b c count)
     (if(= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
   (iter 2 1 0 n))
 
         