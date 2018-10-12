#lang s-exp "typecheck-lang.rkt"
((lambda ([x : Integer]) (+ x 2)) 7)
(define [ft : Integer] 42)
(+ ft 7)

(define [! : (-> Integer Integer)]
  (lambda ([x : Integer]) (if (= x 0) 1 (* x (! (- x 1))))))
(! 6)

(define [canis : String] "dogs")
(define [troo : Boolean] #t)


(define [str-fst : (-> String String)]
  (lambda ([s : String]) (substring s 0 1)))

(str-fst "dogs")

(define [np_or_nq : (-> Boolean Boolean Boolean)]
  (lambda ([p : Boolean] [q : Boolean])
    (not (if p q #f))))

(np_or_nq troo (not troo))

(display "dogs\n")

(define [str! : (-> String Integer)]
  (lambda ([s : String])
    (! (string-length s))))

(str! "caner")

;; ============ PAIRS =============

(define [pr1 : (Pair Integer String)]
  (cons 3 "caner"))

(define [get-sname : (-> (Pair Integer String) String)]
  (lambda ([pr : (Pair Integer String)])
    (cdr pr)))

(get-sname pr1)

(define [get-sid : (-> (Pair Integer String) Integer)]
  (lambda ([pr : (Pair Integer String)])
    (car pr)))

(get-sid pr1)


(define [sqr-cddr : (-> (Pair String (Pair Boolean Integer)) Integer)]
  (lambda ([pr : (Pair String (Pair Boolean Integer))])
    (let ([d (cdr (cdr pr))])
      (* d d))))

(sqr-cddr (cons "word" (cons #t 5)))