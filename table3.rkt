(define (assocc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else
         (assocc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table)))
    
    (define (lookup key1 key2)
      (let ((subt (assocc key1 (cdr local-table))))
        (if subt
            (let ((record (assocc key2 (cdr subt))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert key1 key2 value)
      (let ((subt (assocc key1 (cdr local-table))))
        (if subt
            (let ((record (assocc key2 (cdr subt))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subt (cons (cons key2 value) (cdr subt)))))
            (set-cdr! local-table (cons (cons key1 (list (cons key2 value))) (cdr local-table))))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert)
            (else #f)))
  dispatch))    
    
    
(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert))


(define coercion-table (make-table))
(define put-coercion (coercion-table 'insert))
(define get-coercion (coercion-table 'lookup))


;直角坐标系安装包
  
(define (install-rect-package) 
  (define (square x) (* x x))
  (define (Real-part z) (car z))
  (define (Imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (Magnitude z)
    (sqrt (+ (square (Real-part z))
           (square (Imag-part z)))))
  (define (Angle z)
    (atan (Imag-part z) (Real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  (define (tag x) (attach-tag 'rectangular x))
  (put 'Real-part '(rectangular) Real-part)
  (put 'Imag-part '(rectangular) Imag-part)
  (put 'Magnitude '(rectangular) Magnitude)
  (put 'Angle '(rectangular) Angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y)
         (tag (make-from-mag-ang x y)))))
  
  
  
;极坐标系安装包
(define (install-polar-package) 
  (define (square x) (* x x))
  (define (Real-part z)
    (* (Magnitude z) (cos (Angle z))))
  (define (Imag-part z) 
    (* (Magnitude z) (sin (Angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y))) (atan y x))) 
  (define (Magnitude z) (car z))
  (define (Angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'Real-part '(polar) Real-part)
  (put 'Imag-part '(polar) Imag-part)
  (put 'Magnitude '(polar) Magnitude)
  (put 'Angle '(polar) Angle)
  (put 'make-from-real-imag 'polar 
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y)
         (tag (make-from-mag-ang x y)))))


 (define (attach-tag tag content)
   (if (number? content)
       content
       (cons tag content)))
 
 (define (type-tag datum)
   (cond ((number? datum) 'scheme-number)
         ((pair? datum) (car datum))
         (error "bad tagged datum" datum)))
 
 (define (contents datum)
   (cond ((number? datum) datum)
         ((pair? datum) (cdr datum))
         (error "bad tagged datum" datum)))
 
 
 
 

(define (aReal-part z) (apply-generic 'real-part z))
(define (aImag-part z) (apply-generic 'imag-part z))
(define (aMagnitude z) (apply-generic 'magnitude z))
(define (aAngle z) (apply-generic 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))


(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
 

;(define (add-complex x y)
;  (make-from-real-imag (+ (areal-part x) (areal-part y))
;                      (+ (aimag-part x) (aimag-part y))))
;(define (sub-complex x y)
;  (make-from-real-imag (- (areal-part x) (areal-part y))
;                       (- (aimag-part x) (aimag-part y))))

;(define (mul-complex x y)
 ; (make-from-mag-ang (* (amagnitude x) (amagnitude y))
  ;                   (+ (aangle x) (aangle y))))
;(define (div-complex x y)
 ; (make-from-mag-ang (/ (amagnitude x) (amagnitude y))
  ;                   (- (aangle x) (aangle y))))

(install-rect-package)
(install-polar-package)

(define a (make-from-real-imag 3 4))
(define b (make-from-real-imag 8 12))
;(define c (make-from-mag-ang 1 1))

;;通用型算术运算
(define (apply-generic op . args)
   (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (if (= (length args) 2)
               (let ((type1 (car type-tags))
                     (type2 (cadr type-tags))
                     (a1 (car args))
                     (a2 (cadr args)))
                 (let ((t1->t2 (get-coercion type1 type2))
                       (t2->t1 (get-coercion type2 type1)))
                   (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                         (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                         (else 1))))
               1)))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic 'zero? x))



(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'zero? '(scheme-number) 
       (lambda (x) (= x 0)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))


;;安装有理数包

(define (install-rat-package)
  (define (make-rat x y)
    (let (( a (gcd x y)))
      (if (< (* x y) 0)
          (cons (- (abs (/ x a)))
              (abs (/ y a)))
          (cons (/ x a) (/ y a)))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (add-rat a b)
    (make-rat (+ (* (numer a) (denom b)) (* (denom a) (numer b)))
              (* (denom a) (denom b))))
  
  (define (tag x) (attach-tag 'rat x))
  (put 'numer 'rat numer)
  (put 'denom 'rat denom)
  (put 'add '(rat rat)
       (lambda (x y) (tag (add-rat x y))))
  (put 'make 'rat 
       (lambda (x y) (tag (make-rat x y))))
  'done)

(install-rat-package)
(define make-rat (get 'make 'rat))
(define numer (get 'numer 'rat))
(define denom (get 'denom 'rat))
(define r1 (make-rat 2 5))
(define r2 (make-rat 3 5))

  





;;复数安装包
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex x y)
    (make-from-real-imag (add (areal-part x) (areal-part y))
                      (+ (aimag-part x) (aimag-part y))))
  (define (sub-complex x y)
    (make-from-real-imag (- (areal-part x) (areal-part y))
                       (- (aimag-part x) (aimag-part y))))

  (define (mul-complex x y)
    (make-from-mag-ang (* (amagnitude x) (amagnitude y))
                     (+ (aangle x) (aangle y))))
  (define (div-complex x y)
    (make-from-mag-ang (/ (amagnitude x) (amagnitude y))
                     (- (aangle x) (aangle y))))
  
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (div (add-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'real-part '(complex) areal-part)
  (put 'imag-part '(complex) aimag-part)
  (put 'magnitude '(complex) amagnitude)
  (put 'angle '(complex) aangle)
  (put 'equ? '(complex complex) 
       (lambda (x y) (and (= (areal-part x) (areal-part y)) (= (aimag-part x) (aimag-part y)))))
  (put 'zero? '(complex) 
       (lambda (x) (and (= (areal-part x) 0) (= (aimag-part x) 0))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-real-imag 3 4))
(define c3 (make-complex-from-real-imag 0 0))



;;强制
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(define (rat->scheme-number n)
  (make-scheme-number (/ (/ (numer (contents n)) 1.0) (denom (contents n)))))
(define (rat->complex n)
  (let ((nn (rat->scheme-number n)))
    (scheme-number->complex nn)))

(put-coercion 'scheme-number 'complex scheme-number->complex) 
(put-coercion 'rat 'scheme-number rat->scheme-number)
(put-coercion 'rat 'complex rat->complex)




;;多项式安装包

(define (install-poly-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (display "poly not in same variable")))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (display "poly not in same variable")))

  (define (tag x) (attach-tag 'ploy x))
  (put 'add '(poly poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(poly poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'poly
       (lambda (var term-list) (tag (make-poly var term-list))))
  'done)

(define (add-term l1 l2)
  (cond ((empty-term? l1) l2)
        ((empty-term? l2) l1)
        (else
         (let ((t1 (fist-term l1))
               (t2 (fist-term l2)))
           (cond ((> (order l1) (order l2))
                  (adjoin-term t1
                               (add-term (rest-term l1) l2)))
                 ((< (order l1) (order　l2))
                  (adjoin-term t2
                               (add-term l1 (rest-term l2))))
                 (else
                  (adjoin-term (make-term (order l1)
                                          (add (coeff t1) (coeff t2)))
                               (add-term (rest-term l1)
                                         (rest-term l2)))))))))
 
(define (mul-term l1 l2)
  (if (empty-term? l1)
      (the-empty-termlist)
      (add-term (mul-term-by-all-terms (frist-term l1) l2)
                (mul-term (rest l1) l2))))


(define (mul-term-by-all-terms t l)
  (if (empty-term? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
         (make-term (+ (order t) (order t2))
                    (mul (coeff t) (coeff t2)))
         (mul-term-by-all-terms t (rest-term l))))))
      
  
                          