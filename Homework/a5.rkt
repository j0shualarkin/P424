#lang racket

(require (for-syntax syntax/parse racket/list)
         (rename-in racket (let orig-let)))
(provide (rename-out [let let]))

(begin-for-syntax
  (define-syntax-class const
    (pattern c:number)
    (pattern c:string)
    (pattern c:boolean)))

(define-for-syntax (replace stx id const-expr)
  (cond
    [(and (identifier? stx) (free-identifier=? stx id)) const-expr]
    [else (let ([stx-lst (syntax->list stx)])
            (if stx-lst
                (datum->syntax stx (map (λ (stx)
                                          (replace stx id const-expr))
                                        stx-lst))
                stx))]))

(define-syntax (let stx)
  (syntax-parse stx
    [(_ ([x:id rhs:const] binding ...) body)
     (define body^ (syntax-e (replace #'body #'x #'rhs)))
     #`(let (binding ...) #,body^)]
    [(_ ([x:id rhs:expr] ...) body) #'(orig-let ([x rhs] ...)
                                        body)]))

(let ([x 10]
      [y 20]
      [z "20"])
  (+ x y (string->number z)))
