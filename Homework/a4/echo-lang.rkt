#lang racket


(module echo racket
  (require
  (for-syntax syntax/parse)
  (rename-in racket (#%module-begin modbeg)))
  (provide #%module-begin
           #%app
           #%datum
           #%top
           + > *
           λ if zero? sub1)
  
  (define-syntax (#%module-begin stx)
    (syntax-parse stx
      [(_) #'(void)]
      [(_ e ...) #`(modbeg (begin (println (syntax->datum (syntax e))) e) ... )])))


(module test (submod ".." echo)
  (+ 2 3)
  (+ 4 6)
  (> 2 3)
  (((λ (f) (λ (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
   (λ (f) (λ (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
   5))