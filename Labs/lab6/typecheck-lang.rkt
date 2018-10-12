#lang racket
(require (for-syntax syntax/parse racket/syntax "typechecker.rkt"))
(provide (rename-out [mb #%module-begin]
                     [typed-lambda lambda]
                     [ti #%top-interaction]
                     [typed-define define])
         -> 
         + quote letrec let #%datum #%app print-e if - * =
         substring not
         display string-length
         cons car cdr)

(define-syntax (mb stx)
  (syntax-parse stx
    [(_ e ...)
     (define/with-syntax (mb e* ...)
       (local-expand #'(#%plain-module-begin e ...)
                     'module-begin null))
     
     (define env
       (for/fold ([env empty-env])
                 ([e (syntax->list #'(e* ...))])
         (typecheck-top e env)))
     
     #`(#%module-begin e* ... (printf "types were ~s\n" '#,env))]))

(define-syntax (ti stx)
  (syntax-parse stx
    [(_ . e)
     (define expanded (local-expand #'e 'expression null))
     
     (define t (typecheck expanded empty-env))
     
     #`(#%top-interaction . (begin (printf "type was ~s\n" '#,t) #,expanded))]))

(define-syntax (typed-lambda stx)
  (syntax-parse stx
    [(_ ([i:id : ty] ...) body)
     #'(#%plain-lambda (i ...)
                       (list 'ty ...)
                       body)]))

(define-syntax (typed-define stx)
  (syntax-parse stx
    [(_ [i : t] body)
     #'(define i
         (begin 't
                body))]))

(define-syntax (print-e stx)
  (printf "~s\n" (syntax->datum stx))
  #'5)