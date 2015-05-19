#lang racket
(require scheme/mpair)

(define (pair->m p)
  (if (null? p)
      '()
      (mcons (car p) (pair->m (cdr p)))))

(define-syntax-rule (apply-in-underlying-scheme x y)  (apply x y))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((begin? exp)
         (eval-sequence (begin-action exp) env))
        ((and? exp) (eval-and (cdr exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (applyy (eval (operator exp) env)
                (list-of-value (operands exp) env)))
        (else
         (error "Unkonw expression type--EVAL" exp))))
  
  (define (applyy procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
          (else
           (error "Unkonw procedure type--APPLY" procedure))))
          
 
  
 
  
  

(define (list-of-value exps env)
   (if (no-operand? exps)
       '()
       (let ([right (list-of-value (rest-operand exps) env)])
       (cons (eval (first-operand exps) env)
             right))))
          
 
 (define (eval-if exp env)
   (if (true? (eval (if-predicate exp) env))
       (eval (if-consquent exp) env)
       (eval (if-alternative exp) env)))
 

 (define (eval-sequence exps env)
   (cond ((last-exp? exps) (eval (first-exp exps) env))
         (else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exps) env))))
 
 (define (eval-assignment exp env)
   (set-variable-value! (assignment-variable exp)
                        (eval (assignment-value exp) env)
                        env)
   'ok)
 
 
 ;;
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                     env)
  'ok)


(define (eval-and exps env)
  (if (null? exps)
      true
      (if (false? (eval (first-exp exps) env))
          false
          (eval-and (rest-exps exps) env))))
          

    
 ;;表达式的表示    
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))



(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))



(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))



(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp)  (cadr exp))
(define (lambda-body exp)  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (let? exp) (tagged-list? exp 'let))
(define (let-body exps) (cddr exps))

(define (let-vars exps)
  (let ([var-exp (cadr exps)])
    (define (iter exp)
      (if (null? exp)
          '()
          (cons  (caar exp) (iter (cdr exp)))))
    (iter var-exp)))


(define (let-exps exps)
  (let ([var-exp (cadr exps)])
    (define (iter exp)
      (if (null? exp)
          '()
          (cons  (cadar exp) (iter (cdr exp)))))
    (iter var-exp)))

(define (let->combination exps)
  (cons (make-lambda (let-vars exps) (let-body exps)) (let-exps exps)))

;;
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consquent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consquent alternative)
  (list 'if predicate consquent alternative))




(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-action exp) (cdr exp))
(define (last-exp? sqe) (null? (cdr sqe)))
(define (first-exp sqe) (car sqe))
(define (rest-exps sqe) (cdr sqe))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operand? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))



(define (cond? exp) (tagged-list? exp 'cond))

(define (cond->if exps)
  (let ([exp (cdr exps)])
    (define (iter expp)
      (if (null? (cdr expp))
          (sequence->exp (cdar expp))
          (cons 'if
                (cons (caar expp)
                      (cons (sequence->exp (cdar expp))
                            (cons (iter (cdr expp)) null))))))
    (iter exp)))


(define (and? exp) (tagged-list? exp 'and))



(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))







;;;过程表示

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (mnull? list) 1)
;;;对环境的操作

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define empty-environment '())

(define (make-frame vars vals)
  (mcons vars vals))
(define (frame-variable frame) (mcar frame))
(define (frame-value frame) (mcdr frame))
(define (add-binding-to-frame var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))


(define (extend-environment vars vals base-env)
  (if (pair? vars)
        (let ([var (pair->m vars)] [val (pair->m vals)])
              (if (= (mlength var) (mlength val))
                  (cons (make-frame var val) base-env)
                  (error "arguments error" vars)))
        (if (= (mlength vars) (mlength vals))
            (cons (make-frame vars vals) base-env)
            (error "arguments error" vars))))
              
  

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals )
      (cond ((null? vars)           
            (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  (mcar vals))            
            (else (scan (mcdr vars) (mcdr vals)))))
     (if (eq? env empty-environment) 
        (error "unbound varible" var)
        (let ((frame (first-frame env)))
          (scan (frame-variable frame)
                (frame-value frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals )
      (cond ((null? vars)           
            (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  (set-mcar! vals val))            
            (else (scan (mcdr vars) (mcdr vals)))))
     (if (eq? env empty-environment) 
        (error "unbound varible" var)
        (let ((frame (first-frame env)))
          (scan (frame-variable frame)
                (frame-value frame)))))
  (env-loop env))



  

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame var val frame))
            ((eq? var (mcar vars)) (set-mcar! vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variable frame) (frame-value frame))))







(define (primitive-procedure? pro)
  (tagged-list? pro 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (mlist (mlist 'car car)
        (mlist 'cdr cdr)
        (mlist 'cons cons)
        (mlist 'null? null?)
        (mlist '+ +)
        (mlist '- -)
        (mlist '* *)
        (mlist '/ /)
        (mlist '= =)
        (mlist 'display display)))

(define primitive-procedure-name
  (mmap mcar 
       primitive-procedures))
(define primitive-procedure-objects
  (mmap (lambda (pro)
          (list 'primitive (mcar pro)))
        (mmap mcdr 
                   primitive-procedures)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedure-name
                             primitive-procedure-objects
                             empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define g (setup-environment))




(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))








;;;驱动循环
(define input-prompt ";;;M-eval input:")
(define output-prompt ";;;M-eval output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input g)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))



;;;;;测试


(define b empty-environment)
(define be (extend-environment (mlist 'a 'b)
                               (mlist 1 2)
                               b))
(define be-frame (first-frame be))
(define be-variable (frame-variable be-frame))
(define be-value (frame-value be-frame))


(eval '(define (f x) (+ x 1)) g)
(define v (lookup-variable-value 'f g))


(define e 
    (extend-environment
             (procedure-parameters v)
             '(3)
             (procedure-environment v)))

(define eframe (first-frame e))
(define evar (frame-variable eframe))
(define evall (frame-value eframe))


(define (gvar) (display (frame-variable (first-frame g))))
(define (gval) (display (frame-value (first-frame g))))
(eval '(define r 1) g)


(eval '(define (t x)
         (define (k y) (+ y 1))
         (k x)) g)
