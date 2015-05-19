;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")))))
; An Xexpr.v2 is 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))



; An Xexpre.v2 is 
; - (cons Symbol LX)
; - (cons Symbol (cons LA LX))
; An LX is 
; - empty
; - (cons Xepre.v2 LX)
; An LA is
; - empty
; - (cons Attribute LA)

; An attribute is
; - (cons Symbol (cons String))


(define a0 '((initial "red")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))


; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
;(check-expect (xexpr-attributes e0) '())
;(check-expect (xexpr-attributes e1) '((initial "red")))
;(check-expect (xexpr-attributes e2) '())
;(check-expect (xexpr-attributes e3) '())
;(check-expect (xexpr-attributes e4) '((initial "red")))

(define (xexpr-attributes.v1 xe)
  (local ((define second (cdr xe)))
    (cond 
      [(or (empty? second) (empty? (car second)) (symbol? (caar second))) '()]
      [else (car second)])))
                
                           
(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-lox (first optional-loa+content)))
              (if (list-of-attributes? loa-or-lox)
                  loa-or-lox
                  '()))])))

; [List-of Attribute] or [List-of Xexpr.v2] -> Boolean
; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpre.v2 -> Symbol
(define (xexpr-name xe) (car xe))

; Xexpre.v2 -> [list-of Xexpre.v2]
;(check-expect (xexpr-content e0) '())
;(check-expect (xexpr-content e1) '())
;(check-expect (xexpr-content e2) '((action)))
;(check-expect (xexpr-content e3) '((action)))
;(check-expect (xexpr-content e4) '((action) (action)))
(define (xexpr-content xe)
    (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-lox (first optional-loa+content)))
              (if (list-of-attributes? loa-or-lox)
                  (cdr optional-loa+content)
                  optional-loa+content))])))

; [list-of Attribute] symbol -> String
;(check-expect (attribute-value a0 'initial) "red")
(define (attribute-value la s) (cadr (assq s la)))

; A XWord is '(word ((text String)))
; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord empty))
; – (cons 'li (cons [List-of Attribute] (cons XWord empty)))

(define ee
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define i0 '(li (word ((text "one")))))
; XItem.v1 -> Image 
; renders a single item as a "word" prefixed by a bullet
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define word-in-i (word-text element)))
    (beside/align 'center BULLET (text word-in-i 12 'black))))

(define (word-text i)
  (attribute-value (xexpr-attributes i) 'text))

; XEnum.v1 -> Image 
; renders a simple enumeration as an image
;(check-expect (render e0) e0-rendered)
 
(define (render-enum1 xe)
  empty-image)

(define SIZE 12)
(define COLOR 'black)
(define BULLET
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))

(define item1-rendered
  (beside/align 'center BULLET (text "one" 12 'black)))
(define item2-rendered
  (beside/align 'center BULLET (text "two" 12 'black)))
(define e0-rendered
  (above/align 'left item1-rendered item2-rendered))

; A FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons (tech "FSM-State") (cons (tech "FSM-State") empty))
; A FSM-State is a String that specifies color
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  ; State of the World: FSM-State
  (big-bang state0
            [to-draw
             (lambda (current)
               (square 100 "solid" current))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))
 
; [List-of (cons X (cons Y empty))] X -> Y
; finds the matching Y for the given X in the association list
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "next state not found"))))


; An XMachine is:
;   (list 'machine (list (list 'initial FSM-State)) [List-of X1T])
; An X1T is
;   (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))


(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))


; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm->transitions xm) (xm-state0 xm)))
 
; XMachine -> FSM-State 
; extracts and translate the transition table from a configuration
 
(check-expect (xm-state0 xm0) "red")
 
(define (xm-state0 xm0)
  (attribute-value (xexpr-attributes xm0) 'initial))
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from an XML configuration
 
(check-expect (xm->transitions xm0) fsm-traffic)
 
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (attribute-value (xexpr-attributes xa) 'state)
                  (attribute-value (xexpr-attributes xa) 'next))))
    (map xaction->action (xexpr-content xm))))