#lang racket

(require (for-syntax syntax/parse
                     racket/list)
         rackunit)

;; ----------------------------------------------

(define-syntax and2
  (syntax-parser 
    ((_ a b) #'(if a (if b #t #f) #f))))

(define-syntax and*
  (syntax-parser
    ((_ a) #'(if a a #f))
    ((_ a b ...) #'(if a (and* b ...)  #f))))

(check-equal? (and2 #t #f) #f)
(check-equal? (and* #t #f #f) #f)
(check-equal? (and* #t #t #t #t #t) #t)

;; ----------------------------------------------

(define-syntax when
  (syntax-parser
    ((_ condition e0 ...) #'(if condition (begin e0 ...) (void)))))


(let ((begin list))
  (when #t (displayln "win") (newline)))

(define-syntax unless
  (syntax-parser
    ((_ q e0  ...) #'(when (not q) e0 ...))))


(let ((not (lambda (x) x))
      (when 'never))
  (unless #f (displayln "win") (newline)))

;; ----------------------------------------------


(define-syntax or*
  (syntax-parser
    ((_) #'#f)
    ((_ a) #'(if a a #f))
    ((_ a b ...) #'(if a a (or* b ...)))))

(check-equal? (or* #t #f) #t)
(check-equal? (or* #t #f #f) #t)
(check-equal? (or* #t #t #t #t #t) #t)

(check-equal? (or* #f #f #f) #f)
(check-equal? (or* #f 42 #f) 42)

(check-equal? (or* #f 2 (/ 1 0)) 2)


;; ----------------------------------------------

(define-syntax let1
  (syntax-parser
   [(_ ((i v) ...) e1 e2 ...)
    #'((lambda (i ...) e1 e2 ...) v ...)]))


(let1 ([x 6]) x)

(define-syntax let2
  (syntax-parser
    ;; name provided
    [(_ proc-id ((i v) ...) e1 e2 ...)
     #'(letrec ([proc-id (λ (i ...) e1 e2 ...)])
         (proc-id v ...))]
    ;; same as let1
    [(_ ((i v) ...) e1 e2 ...)
     #'((lambda (i ...) e1 e2 ...) v ...)]))


(define Y!
  ((λ (f)
    (λ (n)
      ((f f) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
  (λ (f)
    (λ (n)
      ((f f) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))))


(define (! n)
  (if (zero? n) 1 (* n (! (sub1 n)))))

(check-equal?
 (let2 fact ([n 10])
       (if (zero? n)
           1
           (* n (fact (sub1 n)))))
 (! 10))


;; -------------------------------------------------------------



(define-syntax let3
  (syntax-parser
    ;; looking for duplicates
    ;; let2
    [(_ proc-id ((i v) ...) e1 e2 ...)
     (if (not (check-duplicates (syntax->list #'(i ...)) free-identifier=?))
     #'(letrec ([proc-id (λ (i ...) e1 e2 ...)])
         (proc-id v ...))
     (raise-syntax-error 'let3 "bindings had duplicates!"))]
    ;; same as let1
    [(_ ((i v) ...) e1 e2 ...)
     #'((lambda (i ...) e1 e2 ...) v ...)]))

#;(let3 ([a 5]
       [b 6]
       [a 7])
      b)

;; --------------------------------------------------------------

(define-syntax cond1
  (syntax-parser
    ((_ [Q C]) #'(if Q C (raise-syntax-error 'cond "no cond clauses matched")))
    ((_ [Q C]
        [Q2 C2]
        ...) #'(if Q C (cond1 [Q2 C2] ...)))))


(check-equal? (cond1 [#t 10]) 10)

(check-equal? (cond1 [#f 10]
                     [#t 11]) 11)


;; =======

(define-syntax cond2
  (syntax-parser 
    ((_ [Q E]) #'(if (equal? (syntax-e Q) 'else*)
                        E
                        #'(if Q E (raise-syntax-error 'cond "no cond clauses matched"))))
    ((_ [Q C]
        [Q2 C2]
        ...) #'(if Q C (cond1 [Q2 C2] ...)))))

(check-equal?
 (cond2
  [#f 10]
  [#f 11]
  [else* 12])
 12)











