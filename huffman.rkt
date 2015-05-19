#lang racket

;about leaf
(define (make-leaf symbol weigth)
  (list 'leaf symbol weigth))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weigth-leaf x) (caddr x))


;about tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbol left) (symbol right))
        (+ (weigth left) (weigth right))))

(define (left-branch t) (car t))
(define (right-branch t) (cadr t))

(define (symbol t)
  (if(leaf? t)
     (list (symbol-leaf t))
     (caddr t)))
(define (weigth t)
  (if(leaf? t)
     (weigth-leaf t)
     (cadddr t)))


;generate huffman-tree

(define (generate-huffman-tree pairs)
  (merge (make-leaf-set pairs)))

(define (merge set)
  (if(= (length set) 1)
     (car set)
     (merge (join (make-code-tree (car set) (cadr set)) (cddr set))))) 
  




;decode 
(define (decode bits tree)
  (define (d bits current)
    (if(null? bits)
       null
       (let ((next (choose (car bits) current)))
         (if(leaf? next)
            (cons (symbol-leaf next) (d (cdr bits) tree))
            (d (cdr bits) next)))))
  (d bits tree))

(define (choose bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit ----CHOOSE" bit))))

;encode
(define (encode message tree)
  (if(null? message)
     null
     (append (encode-symbol (car message) tree) 
             (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (cond ((leaf? tree) null)
        ((element-of-set? char (symbol (left-branch tree))) (cons 0 (encode-symbol char (left-branch tree))))
        ((element-of-set? char (symbol (right-branch tree))) (cons 1 (encode-symbol char (right-branch tree))))
        (else (error "bad char---ENCODE-SYMBOL" char))))


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;about set
(define (join x set)
  (cond ((null? set) (list x))
        ((< (weigth x) (weigth (car set))) (cons x set))
        (else (cons (car set)
                    (join x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      null
      (join (make-leaf (car (car pairs)) (cadr (car pairs)))
            (make-leaf-set (cdr pairs)))))
  
  
  (define h (generate-huffman-tree '((a 8) (b 9) (c 7) (d 9) (e 10))))