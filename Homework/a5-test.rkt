#lang s-exp "a5.rkt"
(+ 10 20) ;; 30
(let ([x 10]
      [y 20]
      [w (+ 20 30)])
  (let ([k w])
    ((lambda (m n) (+ 50 m n x y k))
     (+ x y)
     100))) ;; 260

(let ([z "hello"])
  10
  z)
