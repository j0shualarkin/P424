#lang racket
(module Encode racket
  (provide (rename-out [my-datum #%datum]) #%module-begin #%app)
  (require (for-syntax syntax/parse )
           (rename-in racket [#%datum orig-datum]))
  (define-syntax (my-datum stx)
    (syntax-parse stx
      [(_ . a:number)
       (define v (syntax-e #'a))
       (if (or (<= 65 v 90) (<= 97 v 122))
           #`(orig-datum . #,(string (integer->char v)))
           (raise-syntax-error 'datum "not valid number ~v" #'a))])))

(module test (submod ".." Encode)
  65
  66
  67
  68
  50)
