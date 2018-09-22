#lang racket

(require (for-syntax syntax/parse
                     racket/list)
         (rename-in racket (#%module-begin modbeg)))

(provide #%module-begin
         #%top
         #%app
         #%datum
         * + define)

(define-for-syntax (rearrange stx)
  (define (define=? e) (eqv? (car (syntax->datum e)) 'define))
  (let-values (((xs ys) (partition define=? (syntax->list stx))))
    (append xs ys)))
  
(define-syntax #%module-begin 
  (syntax-parser
    ((_ e ...) #`(modbeg #,@(rearrange #'(e ...))))))

(module+ test 
  (foo 5)
  (define (foo x) (* x 2))
  (bar 16)
  (define (bar y) (+ y (foo y))))