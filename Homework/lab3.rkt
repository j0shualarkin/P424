#lang racket
(require (for-syntax syntax/parse))

(define-syntax loop
  (syntax-parser
    [(k e ...)
     #'(let f () e ... (f))]))

(loop
   (displayln "Outta")
   (newline)
   break
   (displayln "Time"))

delay
