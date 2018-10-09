#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse racket/list)
         (rename-in racket (let orig-let)))
(provide let)

(begin-for-syntax
  (define-syntax-class const
    (pattern c:number)
    (pattern c:string)
    (pattern c:boolean)
    (pattern c:string))
  (define (subst stx var-id const-expr)
    (syntax-parse stx #:literals (λ)
                  [var:id #:when (free-identifier=? #'var var-id) const-expr]
                  [(λ (x:id ...) body) #`(λ (x ...) #,(subst #'body var-id const-expr))]
                  [(e0 er ...) (define data (map (λ (x) (subst x var-id const-expr))
                                                 (syntax->list #'(e0 er ...))))
                               #`(#,@data)]
                  [_ stx]))

  (define (subst-bindings bindings body store)
    (syntax-parse bindings #:literals (λ)
                  [() (values body store)]
                  [([x:id rhs:const] r ...)
                   (subst-bindings #'(r ...) (subst body #'x #'rhs) store)]
                  [([x:id y:id] r ...) (subst-bindings #'(r ...) (subst body #'x #'y) store)]
                  [([x:id rhs:expr] r ...)
                   (subst-bindings #'(r ...) body (cons (list #'x #'rhs) store))])))


(define-syntax (let stx)
  (syntax-parse stx
    [(_ bindings body)
     (define-values (body^ bindings^) (subst-bindings #'bindings #'body '()))
     (if (empty? bindings^) #`#,body^
         #`(orig-let #,bindings^ #,body^))]))

;; (syntax-parse #'()
;;   ['() 10])

(module+ test
  (require rackunit)
  (check-equal? (let ([x 10]
                      [y 20]
                      [w (+ 20 30)]
                      [z "20"])
                  (let ([k w])
                    ((λ (m n) (+ 50 m n x y k))
                     (+ x y (string->number z))
                     100)))
                280)

  (check-equal? (let ([x 10]
                      [y 20]
                      [w 20]
                      [z "20"])
                  ((λ (m n) (+ 50 m n x y w))
                   (+ x y (string->number z))
                   100))
                250))
