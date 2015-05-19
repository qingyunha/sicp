#lang racket


(define (make-machine ops controller-text)
  (let ([machine (make-new-machine)])
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
      (set! s (cons x s)))
    (define (pop x)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ([top (car s)])
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispacth m)
      (cond ((eq? m 'push) push)
            ((eq? m 'pop) (pop))
            ((eq? m 'initialize) (initialize))
            (else (error "Unknown request -- STACK" m))))
    dispacth))


(define (pop stack) (stack 'pop))
(define (push stack) (stack 'push))



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
              (let ([reg (make-register name)])
                (set! register-table (cons (list name reg) register-table))
                reg))))
      
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
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
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
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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




;;gcd
(define gcd 
  (make-machine 
   (list (list 'rem remainder) (list '= =) (list 'display display) (list 'read read))
   '(
     (assign a (op read))
     (assign b (op read))
     test-b
     (test (op =) (reg b) (const 0))
     (branch (lable gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (lable test-b))
     gcd-done
     (perform (op display) (reg a))
     )))



