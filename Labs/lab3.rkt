#lang racket

(require (for-syntax syntax/parse)
         (rename-in racket
                    (<     rkt-<)
                    (+     rkt-+)
                    (#%app rkt-app)
                    (car   rkt-car)
                    (cdr   rkt-cdr)
                    (cons  rkt-cons)
                    (null  rkt-null)))




(define-syntax loop
  (syntax-parser 
    [(k e ...) (with-syntax ([break (datum->syntax #'k 'break)])
                 #'(let/cc break
                     (let f () e ... (f))))]
    ))

(loop
   (displayln "Outta")
   (newline)
   (break)
   (displayln "Time"))


#;
(define-syntax loop
  (syntax-parser #:literals (break)
                 [(k (break) e ...) #'(void)]
                 [(k e ...) #'(let f () e ... (f))]
                 ))



(define-syntax #%app
  (syntax-parser
    [(_ f arg ...) #'(#%plain-app f (delay arg) ...)]))

(define-syntax +
  (syntax-parser
    [(_ m n ...) #'(rkt-app rkt-+ (force m) (force n) ...)]))

(define-syntax <
  (syntax-parser
    [(_ m ...) #'(rkt-app rkt-< (force m) ...)]))

#;(define-syntax cons
  (syntax-parser
    [(_ a d) #'(rkt-app rkt-cons (delay a) (delay d))]))

(define-syntax car
  (syntax-parser
    [(_ xs) #'(rkt-app force (rkt-app rkt-car (rkt-app force xs)))]))

(define-syntax cdr
  (syntax-parser
    [(_ xs) #'(rkt-app force (rkt-app rkt-cdr  (rkt-app force xs)))]))


(provide (rename-out #;[my-app #%app]
                     #;[my-+ +])
         + < 
         #%datum #%module-begin
         #%top #%app)


(+ 1 2)
(if (< 1 2) 'yes 'no)
(if (< 2 1) 'yes 'no)

(cons 1 null)

(car (cons 1 null))
(cdr (cons 1 null))
