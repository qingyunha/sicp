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





(define (make-machine register-names ops controller-text)
  (let ([machine (make-new-machine)])
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


;;register

(define (make-register name)
  (let ([contents '*unassignment]
        [race false])
    (define (dispacth m)
      (cond ((eq? m 'get) contents)
            ((eq? m 'set)
             (if race
                 (lambda (value) 
                   (newline) (display name) (display ": ") (display contents) (display "--->>") (display value)
                   (set! contents value))
                 (lambda (value) (set! contents value))))
            ((eq? m 'race-on) (set! race true))
            ((eq? m 'race-off) (set! race false))
            (else 
             (error "Unknown request -- REGISTER" m))))
    dispacth))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))
(define (turn-on reg) (reg 'race-on))
(define (turn-off reg) (reg 'race-off))

;;stack
(define (make-stack)
  (let ([s '()])
    (define (push x)
      ;(display x) (newline)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ([top (car s)])
            (set! s (cdr s))
           ; (display top) (newline)
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispacth m)
      (cond ((eq? m 'push) push)
            ((eq? m 'pop) (pop))
            ((eq? m 'initialize) (initialize))
            ((eq? m 'contents) s)
            (else (error "Unknown request -- STACK" m))))
    dispacth))


(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))



(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()]
        [count 0]
        [race false])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply define register: " name)
            (set! register-table (cons (list name (make-register name))
                                       register-table)))
        'register-allocated)
      
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (error "Unknown register :" name))))
      
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin 
                ((instruction-execution-proc (car insts)))
                (set! count (+ count 1))
                (execute)))))
      
      (define (executee)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin 
                (newline)
                (display (instruction-text (car insts)))
                ((instruction-execution-proc (car insts)))
                (set! count (+ count 1))
                (executee)))))
      
      
      (define (dispatch m)
        (cond ((eq? m 'start)
               (set-contents! pc the-instruction-sequence)
               (if race
                   (executee)
                   (execute)))
              ((eq? m 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? m 'allocate-register) allocate-register)
              ((eq? m 'get-register) lookup-register)
              ((eq? m 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? m 'stack) stack)
              ((eq? m 'operations) the-ops)
              ((eq? m 'instructions) the-instruction-sequence)
              ((eq? m 'register) register-table)
              ((eq? m 'inst-counter) (display count) (set! count 0))
              ((eq? m 'race-on) (set! race true))
              ((eq? m 'race-off) (set! race false))
              ((eq? m 'turn-on-reg) (lambda (name) (turn-on (lookup-register name))))
              (else (error "Unknown request -- MACHINE" m))))
    dispatch)))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (turn-on-reg machine name) ((machine 'turn-on-reg) name))

;;the assembler


(define (extract-lables text)
  (if (null? text)
      (cons '() '())
      (let ([result (extract-lables (cdr text))])
        (let ([insts (car result)] [lables (cdr result)])
          (let ([next-inst (car text)])
            (if (symbol? next-inst)
                (cons insts
                      (cons (make-lable-entry next-inst insts) lables))
                (cons (cons (make-instruction next-inst) insts)
                      lables)))))))

(define (assemble controller-text machine)
  (let ([result (extract-lables controller-text)])
    (let ([insts (car result)] [lables (cdr result)])
      (update-insts! insts lables machine)
      insts)))


(define (update-insts! insts lables machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each 
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) lables machine pc flag stack ops)))
     insts)))


(define (make-instruction text) (mcons text '()))

(define (instruction-text inst) (mcar inst))

(define (instruction-execution-proc inst) (mcdr inst))

(define (set-instruction-execution-proc! inst proc) 
  (set-mcdr! inst proc))

(define (make-lable-entry lable-name insts)
  (cons lable-name insts))

(define (lookup-lable lables lable-name)
  (let ([val (assoc lable-name lables)])
    (if val
        (cdr val)
        (error "Undefined lable -- ASSEMBER" lable-name))))


;;Generating execution procedures for instructions

(define (make-execution-procedure inst lables machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine lables ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine lables ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine lables  flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine lables pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine lables ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))
         
         
;;assign instruction

(define (make-assign inst machine lables ops pc)
  (let ([target (get-register machine (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)])
    (let ([value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine lables ops)
               (make-primitive-exp (car value-exp) machine lables))])
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name ass-inst) (cadr ass-inst))
(define (assign-value-exp ass-inst) (cddr ass-inst))


(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))
         


;;test instruction

(define (make-test inst machine lables ops flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
        (let ([condition-proc (make-operation-exp condition machine lables ops)])
          (lambda()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-inst) (cdr test-inst))



;;branch instruction

(define (make-branch inst machine lables flag pc)
  (let ([dest (branch-dest inst)])
    (if (lable-exp? dest)
        (let ([insts (lookup-lable lables (lable-exp-lable dest))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH -- ASSEMBLE" inst))))

(define (branch-dest branch-inst) (cadr branch-inst))




;;goto instruction
(define (make-goto inst machine lables pc)
  (let ([dest (goto-dest inst)])
    (cond ((lable-exp? dest)
           (let ([insts (lookup-lable lables (lable-exp-lable dest))])
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ([reg (get-register machine (register-exp-reg dest))])
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO -- ASSEMBLE" inst)))))

(define (goto-dest goto-inst) (cadr goto-inst))



;;save and restore instruction
(define (make-save inst machine stack pc)
  ;(display inst) (newline)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  ;(display inst) (newline)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda () 
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name inst) (cadr inst))
      


;;perform instruction

(define (make-perform inst machine lables ops pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
        (let ([action-proc (make-operation-exp
                            action machine lables ops)])
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction-- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))
                



;;excution procedure for subexpresion
(define (make-operation-exp exp machine lables ops)
  (let ([op (lookup-prim (operation-exp-op exp) ops)]
        [aprocs (map (lambda (e)
                       (make-primitive-exp e machine lables))
                     (operation-exp-operands exp))])
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))
  
  (define (lookup-prim symbol ops)
    (let ([val (assoc symbol ops)])
      (if val
          (cadr val)
          (error "Uknown operation -- ASSEMBLE" symbol))))
  
  
  
(define (make-primitive-exp exp machine lables)
  (cond ((constant-exp? exp)
         (let ([c (constant-exp-value exp)])
           (lambda () c)))
        ((lable-exp? exp)
         (let ([insts (lookup-lable lables (lable-exp-lable exp))])
           (lambda () insts)))
        ((register-exp? exp)
         (let ([reg (get-register machine (register-exp-reg exp))])
           (lambda () (get-contents reg))))
        (else
         (error "Unknown expresion type -- ASSEMBLE" exp))))


;;expression syntax


(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

(define (lable-exp? exp) (tagged-list? exp 'lable))
(define (lable-exp-lable exp) (cadr exp))

(define (operation-exp? exp) 
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op exp) (cadar exp))
(define (operation-exp-operands exp) (cdr exp))





(define a
'(
  (perform (op initialize-stack))
  (assign continue (lable signal-right))
 eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (lable ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (lable ev-variable))
  (test (op quoted?) (reg exp))
  (branch (lable ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (lable ev-assignment))
  (test (op definition?) (reg exp))
  (branch (lable ev-definition))
  (test (op if?) (reg exp))
  (branch (lable ev-if))
  (test (op lambda?) (reg exp))
  (branch (lable ev-lambda))
  (test (op begin?) (reg exp))
  (branch (lable ev-begin))
  (test (op application?) (reg exp))
  (branch (lable ev-application))
  (goto (lable unknown-expression-type))))


(define b
'(
 ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
 ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
 ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
 ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))))

(define c
'(
 ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (lable ev-appl-did-operator))
  (goto (lable eval-dispatch))
 
 ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (lable apply-dispatch))
  (save proc)
 ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (lable ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (lable ev-appl-accumulate-arg))
  (goto (lable eval-dispatch))
  
 ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (lable ev-appl-operand-loop))
  
 ev-appl-last-arg
  (assign continue (lable ev-appl-accum-last-arg))
  (goto (lable eval-dispatch))
  
 ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (lable apply-dispatch))))

(define d
'(
 apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (lable primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (lable compound-apply))
  (test (op compiled-procedure?) (reg proc))
  (branch (lable compiled-apply))
  (goto (lable unknown-procedure-type))
  
 primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))
  
 compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (lable ev-sequence))
  
 compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))))


(define e
'(
 ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (lable ev-sequence))
  
 ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (lable ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (lable ev-sequence-continue))
  (goto (lable eval-dispatch))
  
 ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (lable ev-sequence))
  
 ev-sequence-last-exp
  (restore continue)
  (goto (lable eval-dispatch))))

(define f
'(
 ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (lable ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (lable eval-dispatch))
  
 ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (lable ev-if-consequent))
 ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (lable eval-dispatch))
  
 ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (lable eval-dispatch))))

(define g
'(
 ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (lable ev-assignment-1))
  (goto (lable eval-dispatch))
  
 ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))))


(define h
'(
 ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (lable ev-definition-1))
  (goto (lable eval-dispatch))
  
 ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))))

(define A
'(
  (branch (lable external-entry))
 read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op prompt-for-input) (const ";;;EC-eval input"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (lable print-result))
  (goto (lable eval-dispatch))
  
 print-result
 (perform (op announce-output) (const ";;;EC-eval value"))
 (perform (op user-print) (reg val))
 (goto (lable read-eval-print-loop))
 
 external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (lable print-result))
  (goto (reg val))))
          
(define i        
'(
 unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (lable signal-error))
  
 unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (lable signal-error))
  
 signal-error
  (perform (op user-print) (reg val))
  (assign val (const error))
 signal-right
  (assign unev (const right))))





(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))



(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-body procedure-body)
        (list 'extend-environment extend-environment)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'list list)
        (list 'cons cons)
        (list 'car car)
        (list 'cdr cdr)
        (list 'false? false?)))



        
   (define controller (append  A a b c d e f g h i))  
   
   ;(define extraction (extract-lables controller))
   ;(define insts (car (extract-lables controller)))
   ;(define lables (cdr (extract-lables controller)))
   
   ;(define machine (make-new-machine))
   
   ;(for-each (lambda (register-name)
    ;            ((machine 'allocate-register) register-name))
     ;        '(continue exp env val unev proc argl))
              
  ; ((machine 'install-operations) eceval-operations)
   ;((machine 'install-instruction-sequence)
    ; (assemble controller machine))
   ;(define update-insts (update-insts! insts lables machine))
   
   
        
        
 (define eceval
   (make-machine '(continue exp env val unev proc argl)
                 eceval-operations
                 controller))
               




















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
         (map (lambda (operand) (compile operand 'val 'next))
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




;;;;;
(define (compile-and-go expression)
  (let ([instructions 
         (assemble (statement
                    (compile expression 'val 'return))
                   eceval)])
    (set! global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
    


(define aa '(define (factorial n)
                (if (= n 1)
                    1
                    (* (factorial (- n 1)) n))))



;(compile-and-go aa)
