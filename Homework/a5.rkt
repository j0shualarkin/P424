#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse racket/list)
         (rename-in racket (let orig-let)))
(provide (rename-out [myapp #%app])
         let #%module-begin #%top lambda + - * / quotient modulo add1 sub1 abs #%datum)

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
                  [() (cons body store)]
                  [([x:id rhs:const] r ...)
                   (subst-bindings #'(r ...) (subst body #'x #'rhs) store)]
                  [([x:id y:id] r ...) (subst-bindings #'(r ...) (subst body #'x #'y) store)]
                  [([x:id rhs:expr] r ...)
                   (subst-bindings #'(r ...) body (cons (list #'x #'rhs) store))])))


(define-syntax (let stx)
  (syntax-parse stx
    [(_ bindings body r ...)
     (define result (map (λ (bdy) (subst-bindings #'bindings bdy '())) (cons #'body (syntax->list #'(r ...)))))
     (define bindings^ (append* (map cdr result)))
     (define body+^ (map car result))
     (if (empty? bindings^) #`(begin #,@body+^)
         #`(orig-let #,bindings^ #,@body+^))]))

(define-for-syntax optbl (for/hash ([i (in-list '(+ - * / abs))]
                                    [v (in-list (list + - * / abs))])
                           (values i v)))
(define-syntax (myapp stx)
  (syntax-parse stx #:literals (lambda)
                [(_ op n:number r:number ...)
                 (let ([val (apply (hash-ref optbl (syntax->datum #'op)) (syntax->datum #'n) (map syntax->datum (syntax->list #'(r ...))))])
                   #`#,val)]
                [(_ (lambda (x ...) body) rand ...)
                 #'(let ([x rand]
                         ...)
                     body)]
                [(_ op rand ...)
                 #'(orig-#%app op rand ...)]))



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
                  ((lambda (m n) (+ 50 m n x y w))
                   (+ x y (string->number z))
                   100))
                250))
