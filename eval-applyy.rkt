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
         (eval-sequence (begin-actions exp) env))
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
          
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (evall exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else 
         (error "Unknown expression type -- Analyze" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))
         
 
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((var (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure var bproc env))))


(define (analyze-sequence exps)
  (define (sequentially p1 p2)
    (lambda (env) (p1 env) (p2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence --- Analyze")
    (loop (car procs) (cdr procs)))))
        


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc) 
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))))


(define exp 
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-value exps env)
   (if (no-operands? exps)
       '()
       (let ([right (list-of-value (rest-operands exps) env)])
       (cons (eval (first-operand exps) env)
             right))))
          
 
 (define (eval-if exp env)
   (if (true? (eval (if-predicate exp) env))
       (eval (if-consequent exp) env)
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
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consquent alternative)
  (list 'if predicate consquent alternative))




(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
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
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))



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
            ((eq? var (mcar vars)) (set-mcar! vals val))
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
(define global-environment (setup-environment))
(define (get-global-environment) global-environment)



(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (gvar) (display (frame-variable (first-frame global-environment))))
(define (gval) (display (frame-value (first-frame global-environment))))





;;;驱动循环
(define input-prompt ";;;M-eval input:")
(define output-prompt ";;;M-eval output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-environment)))
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










(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))