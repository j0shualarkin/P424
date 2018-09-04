#lang racket


;; An Expression is one of:
;; - (if Expression Expression Expression)
;; - (lambda (Var) Expression)
;; - Variable -- just symbols
;; - Number
;; - (Expression Expression ...)
;; - (MACRO S-Expression ...)

;; - An S-Expression is one of:
;; - Number
;; - Symbol
;; - (S-Expression ...)

(define the-macros (make-hash))
(define (macro? m) (hash-ref the-macros m #f))
(define (get-macro m) (hash-ref the-macros m))

(define (extend-syntax! m proc)
  (hash-set! the-macros m proc))


;; expand : S-Expression -> Expression
;; expand all the macros
(define (expand s)
  (match s
    [(? number?) s]
    [(? symbol?) s]
    [`(lambda (,v) ,body)
    `(lambda (,v) ,(expand body))]
    [`(if ,tst ,thn ,els)
     `(if ,(expand tst) ,(expand thn) ,(expand els))]
    [`(,(? macro? m) ,operand ...)
     (expand ((get-macro m) s))]
    [`(,op ,operand ...)
     `(,(expand op) ,@(map expand operand))]))

(extend-syntax! 'or
                (lambda (s)
                  (match s
                    [`(or ,a ,b)
                     `(let ([x ,a])
                        (if x
                            x
                            ,b))]
                    [`(or ,a ,b ,c)
                     `(or ,a (or ,b ,c))])))



(extend-syntax! 'let
                (λ (s)
                  (match s
                    [`(let ((,x ,v)) ,b)
                      `((lambda (,x) ,b) ,v)])))

(extend-syntax! 'cond
                (λ (s)
                  (match s
                    [`(cond) `(void)]
                    [`(cond [else ,at]) at]
                    [`(cond [,a ,at] . ,c)
                     `(if ,a ,at
                          (cond . ,c))]
                    )))


(define (interp s)
  (let loop ([s (expand s)])
    (match s
      [`(if ,(app loop 0) ,e1 ,e2) (loop e2)]
      [`(if ,(app loop x) ,e1 ,e2) (loop e1)]
      [`(lambda (,x) ,body)
       (lambda (v) (loop (subst x v body)))]
      #;[`(,(? macro? m) ,operand ...)
       (interp ((get-macro m) s))]
      [`(,f ,e ...) (apply (loop f) (map loop e))]
      [(? number?) s])))

(define (subst x v s)
  (match s
    [(== x) v]
    [`(if ,e1 ,e2 ,e3) `(if ,(subst x v e1)
                            ,(subst x v e2)
                            ,(subst x v e3))]
    [`(lambda (,x0) ,b)
     (if (eq? x0 x)
         s
         `(lambda (,x0)
            ,(subst x v b)))]
    [`+ +]
    [(list e ...) (map (lambda (e) (subst x v e)) e)]
    [_ s]))


(module+ test
  (require rackunit)
  (check-equal? (interp '(let ([x 1]) (or x (+ x 2)))) 1)
  (check-equal? (interp '((lambda (x) (or x 2 y)) 0)) 2)
  (check-equal? (interp `(let ([x 10]) (or 0 x))) 0) ;; our macro expander is not hygienic. x in the expander captures the x variable here
  (check-equal? (interp `(cond [1 2]
                               [0 3])) 2)
  (check-equal? (interp `(cond [0 2]
                               [else 3])) 3))
