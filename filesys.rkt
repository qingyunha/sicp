;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname filesys) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")))))
(define (how-many3 dir)
  (cond [(empty? dir) 0]
        [else (+ (how-many-files (dir-files dir))
                 (how-many-dirs (dir-dirs dir)))]))

(define (how-many-files files)
  (cond [(empty? files) 0]
        [else (+ 1 (how-many-files (cdr files)))]))

(define (how-many-dirs dirs)
  (cond [(empty? dirs) 0]
        [else (+ (how-many3 (car dirs))
                 (how-many-dirs (cdr dirs)))]))

(define (how-many3.1 dir)
  (+ (foldl how-many-dirr 0 (dir-dirs dir))
     (foldl how-many-filess 0 (dir-files dir))))

(define (how-many-dirr dir base)
  (+ base (how-many3.1 dir)))

(define (how-many-filess file base)
  (+ base 1))

; A File.v3 is a structure: 
;   (make-file Symbol N String


; A Dir.v3 is a structure: 
;   (make-dir.v3 Symbol Dir* File*)
 
; A Dir* is one of: 
; – empty
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – empty
; – (cons File.v3 File*)

(define (find? dir name)
  (or (find-in-dirs (dir-dirs dir) name)
      (find-in-files (dir-files dir) name)))

(define (find-in-dirs dirs name)
  (ormap (lambda (dir) (find? dir name)) dirs))

(define (find-in-files files name)
  (ormap (lambda (file) (symbol=? (file-name file) name)) files))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ls dir)
  (append (ls-dirs (dir-dirs dir)) (ls-files (dir-files dir))))

(define (ls-dirs dirs)
  (foldl (lambda (dir base) (cons (dir-name dir) base)) empty dirs))

(define (ls-files files)
  (foldl (lambda (file base) (cons (file-name file) base)) empty files))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (du dir)
  (+ (du-dirs (dir-dirs dir)) (du-files (dir-files dir))))

(define (du-dirs dirs)
  (foldl (lambda (dir base) (+ 1 base (du dir))) 0 dirs))

(define (du-files files)
  (foldl (lambda (file base) (+ base (file-size file))) 0 files))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
; Path = [List-of Symbol]
; interpretation directions on how to find a file in a directory tree

(define (ls-R dir)
   (append (map (lambda (path) (cons (dir-name dir) path)) (ls-r-dirs (dir-dirs dir)))
           (map (lambda (path) (cons (dir-name dir) path)) (ls-r-files (dir-files dir)))))

(define (ls-r-dirs dirs)
  (map ls-R dirs))

(define (ls-r-files files)
  (map (lambda (file) (cons (file-name file) empty)) files))
  
 
(define d (create-dir "C:\\Users\\hs\\Desktop\\ruby\\ruby-2.1.2\\bin"))
(define read! (make-file 'read! 19 ""))
(define Docs3 (make-dir 'Docs empty (cons read! empty)))
(define hang (make-file 'hang 8 ""))
(define draw (make-file 'draw 2 ""))
(define Code3 (make-dir 'Code empty (cons hang (cons draw empty))))
(define Libs3 (make-dir 'Libs (cons Docs3 (cons Code3 empty)) empty))
(define part1 (make-file 'part1 99 ""))
(define part2 (make-file 'part1 52 ""))
(define part3 (make-file 'part1 17 ""))
(define Text3 (make-dir 'Text empty (cons part1 (cons part2 (cons part3 empty)))))
(define Ts3 (make-dir 'Ts (cons Text3 (cons Libs3 empty)) (cons read! empty)))