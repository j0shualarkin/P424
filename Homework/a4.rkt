#lang racket
(module Encode racket
  (provide (rename-out [my-datum #%datum]) #%module-begin #%app)
  (require (for-syntax syntax/parse )
           (rename-in racket [#%datum orig-datum]))
  (define-syntax (my-datum stx)
    (syntax-parse stx
      [(_ . a:number) #`(orig-datum . #,(string (integer->char (syntax-e #'a))))])))

(module test (submod ".." Encode)
  65
  66
  67
  68)
