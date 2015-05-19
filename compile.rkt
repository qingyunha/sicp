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
(define global-environment (setup-environment))
(define (get-global-environment) global-environment)



(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (gvar) (display (frame-variable (first-frame global-environment))))
(define (gval) (display (frame-value (first-frame global-environment))))


























(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp) 
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) 
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else 
         (error "Unknown expression type -- COMPLIE" exp))))







(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))



;;;;;;;

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) 
                                    '() 
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '()
                                    '()
                                    `((goto (lable ,linkage)))))))


(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))



;;;;
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '()
                                               (list target)
                                               `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '()
                                               (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env)
                                               (list target)
                                               `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))


;;;;;

(define (compile-assignment exp target linkage)
  (let ([var (assignment-variable exp)]
        [get-value-code
         (compile (assignment-value exp) 'val 'next)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val)
                                                              (list target)
                                                             `((perform (op set-variale-value) (const var) (reg val) (reg env))
                                                               (assign ,target (const ok))))))))



(define (compile-definition exp target linkage)
  (let ([var (definition-variable exp)]
        [get-value-code
         (compile (definition-value exp) 'val 'next)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val)
                                                              (list target)
                                                             `((perform (op define-variable!) (const ,var) (reg val) (reg env))
                                                               (assign ,target (const ok))))))))



;;;;
(define lable-counter 0)
(define (new-lable-number)
  (set! lable-counter (+ 1 lable-counter))
  lable-counter)

(define (make-lable name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-lable-number)))))





    
(define (compile-if exp target linkage)
  (let ([t-branch (make-lable 'true-branch)]
        [f-branch (make-lable 'false-branch)]
        [after-if (make-lable 'after-if)])
    (let ([consequent-linkage
           (if (eq? linkage 'next) after-if linkage)])
      (let ([p-code (compile (if-predicate exp) 'val 'next)]
            [c-code (compile (if-consequent exp) target consequent-linkage)]
            [a-code (compile (if-alternative exp) target linkage)])
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequence (make-instruction-sequence '(val)
                                                                           '()
                                                                           `((test (op false?) (reg val))
                                                                             (branch (lable ,f-branch))))
                                                (parallel-instruction-sequence
                                                 (append-instruction-sequence t-branch c-code)
                                                 (append-instruction-sequence f-branch a-code))
                                                after-if))))))



;;;
(define (compile-sequence exp target linkage)
  (if (last-exp? exp)
      (compile (first-exp exp) target linkage)
      (preserving '(env continue)
                  (compile (first-exp exp) target 'next)
                  (compile-sequence (rest-exps exp) target linkage))))


;;;

(define (compile-lambda exp target linkage)
  (let ([proc-entry (make-lable 'entry)]
        [after-lambda (make-lable 'after-lambda)])
    (let ([lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)])
      (append-instruction-sequence (tack-on-instruction-sequence 
                                    (end-with-linkage lambda-linkage
                                                      (make-instruction-sequence '(env)
                                                                                 (list target)
                                                                                 `((assign ,target (op make-compiled-procedure) (lable ,proc-entry) (reg env)))))
                                    (compile-lambda-body exp proc-entry))
                                   after-lambda))))



(define (compile-lambda-body exp proc-entry)
  (let ([formals (lambda-parameters exp)])
    (append-instruction-sequence
     (make-instruction-sequence '(env proc argl)
                                '(env)
                                `(,proc-entry 
                                  (assign env (op compiled-procedure-env) (reg proc))
                                  (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))



                             
                                                                                           


;;;;
(define (compile-application exp target linkage)
  (let ([proc-code (compile (operator exp) 'proc 'next)]
        [operand-codes
         (map (lambda (operand) (compile operand 'var 'next))
              (operands exp))])
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))



(define (construct-arglist operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (null? operand-codes)
        (make-instruction-sequence '()
                                   '(argl)
                                   '((assign argl (const ()))))
        (let ([code-to-get-last-args (append-instruction-sequence (car operand-codes)
                                                                  (make-instruction-sequence '(val) 
                                                                                             '(argl)
                                                                                             '((assign argl (op list) (reg val)))))])
          (if (null? (cdr operand-codes))
                     code-to-get-last-args
                     (preserving '(env)
                                 code-to-get-last-args
                                 (code-to-rest-args (cdr operand-codes))))))))



(define (code-to-rest-args operand-codes)
  (let ([code-for-next-arg 
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl)
                                                '(argl)
                                                '((assign argl (op cons) (reg val) (reg argl)))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-rest-args (cdr operand-codes))))))




;;;;;

(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-lable 'primitive-branch)]
        [compiled-branch (make-lable 'compiled-branch)]
        [after-call (make-lable 'after-call)])
    (let ([compiled-linkage 
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequence 
       (make-instruction-sequence '(proc)
                                  '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (lable ,primitive-branch))))
       (parallel-instruction-sequence
        (append-instruction-sequence compiled-branch
                                     (compile-proc-appl target compiled-linkage))
        (append-instruction-sequence primitive-branch
                                      (end-with-linkage linkage
                                                        (make-instruction-sequence '(proc argl)
                                                                                   (list target)
                                                                                   `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))))
       after-call))))





;;;
(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc)
                                    all-regs
                                    `((assign continue (lable ,linkage))
                                      (assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ([proc-return (make-lable 'proc-return)])
           (make-instruction-sequence '(proc)
                                      all-regs
                                      `((assign continue (lable ,proc-return))
                                        (assign val (op compiled-procedure-entry) (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (lable ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue)
                                    all-regs
                                    '((assign val (op compiled-procedure-entry) (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))


;;;;;
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statement s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


;;;;
(define (append-instruction-sequence . seq)
  (define (append2 seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statement seq1) (statement seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append2 (car seqs) (append-seq-list (cdr seqs)))))
  (append-seq-list seq))



(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))




(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequence seq1 seq2)
      (let ([first-reg (car regs)])
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statement seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))




(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statement seq) (statement body-seq))))


(define (parallel-instruction-sequence seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statement seq1) (statement seq2))))




(define a (compile
             '(define (factorial n)
                (if (= n 1)
                    1
                    (* (factorial (- n 1)) n)))
             'val
             'next))


(define b (compile
           'a
           'val
           'next))