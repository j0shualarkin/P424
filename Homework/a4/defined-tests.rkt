#lang s-exp "defined-lang.rkt"
;; #lang racket
(foo 5)
(define (foo x) (* x 2))
(bar 16)
(define (bar y) (+ y (foo y)))
