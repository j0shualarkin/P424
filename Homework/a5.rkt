#lang racket

(require (for-syntax syntax/parse racket/list)
         (rename-in racket (let orig-let)))
(provide (rename-out [let let]))

(begin-for-syntax
  (define-syntax-class const
    (pattern c:number)
    (pattern c:string)
    (pattern c:boolean))
  (define (subst stx var-id const-expr)

    (syntax-parse stx #:literals (λ)
      [var:id #:when (free-identifier=? #'var var-id) const-expr]
      [(λ (x:id ...) body) #`(λ (x ...) #,(subst #'body var-id const-expr))]
      [(e0 er ...) (define data (map (λ (x) (subst x var-id const-expr))
                                     (syntax->list #'(e0 er ...))))
                  #`(#,@data)]
      [_ stx])))



(define-syntax (let stx)
  (syntax-parse stx
    [(_ ([x:id rhs:const] binding ...) body)
     (define body^ (syntax-e (subst #'body #'x #'rhs)))
     #`(let (binding ...) #,body^)]
    [(_ ([x:id rhs:expr] ...) body) #'(orig-let ([x rhs] ...)
                                        body)]))

(module+ test
  (require rackunit)
  (check-equal? (let ([x 10]
                      [y 20]
                      [z "20"])
                  ((λ (m n) (+ 50 m n x y))
                   (+ x y (string->number z))
                   100))
                230))
