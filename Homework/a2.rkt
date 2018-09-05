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
    ['() s]
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
#;
((Z-comb (λ (f)
           (λ (x)
             (cond
               ()               [(zero? x) 1]
               [else (* x (f (- x 1)))])))) 10)



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
                          (cond . ,c))])))

(extend-syntax! 'case
                (λ (s)
                  (match s
                    [`(case ,case-s0  . ,rest-case-exprs)
                     ;; case-s0 'x
                     `(let ([case-temp-variable ,case-s0])
                        (cond
                          ,@(map (λ (s)
                                   (match-let ([`[,case-v ,case-v-expr] s])
                                     `[(symbol=? case-temp-variable ,case-v) ,case-v-expr]))
                                 rest-case-exprs)))])))

(extend-syntax! 'letrec
                (λ (s)
                  (match s
                    [`(letrec ([,variable ,expr])
                        ,body)
                     `(let ([,variable (Z-comb (lambda (,variable) ,expr))])
                        ,body)])))

(extend-syntax! 'loop
                (λ (s)
                  (match s
                    [`(loop ,variable0 (,variable1 ,expr1)
                        ,expr2)
                     `(letrec ([,variable0 (lambda (,variable1) ,expr1)])
                        ,expr2)])))

(extend-syntax! 'for/list
                (λ (s)
                  (match s
                    [`(for/list ([,ele ,LIST])
                        ,body)
                     `(letrec ([list-maker (lambda (the-list)
                                             (cond
                                               [(null? the-list) '()]
                                               [else (let ([,ele (car the-list)])
                                                       (cons ,body (list-maker (cdr the-list))))]))])
                        (list-maker ,LIST))])))

(define (Z-comb f)
  ((λ (x) (f (λ (v) ((x x) v))))
   (λ (x) (f (λ (v) ((x x) v))))))


(define (sym=? a b)
  (if (symbol=? a b) 1
      0))

(define (interp s)
  (let loop ([s (expand s)])
    (match s
      ['() '()]
      [`(symbol=? ,e1 ,e2) (sym=? (loop e1) (loop e2))]
      ['+ +]
      ['car car]
      ['cdr cdr]
      ['cons cons]
      [`(null? ,l) (if (null? (loop l)) 1 0)]
      ['* *]
      ['- -]
      [`Z-comb Z-comb]
      [(? symbol?) s]
      [`(if ,(app loop 0) ,e1 ,e2) (loop e2)]
      [`(if ,(app loop x) ,e1 ,e2) (loop e1)]
      [`(lambda (,x) ,body)
       (lambda (v)
         (loop (subst x v body)))]
      #;[`(,(? macro? m) ,operand ...)
         (interp ((get-macro m) s))]
      [`(list ,r ...) r]
      [`(quote ,e) e]
      [`(,f ,e ...) (apply (loop f) (map loop e))]
      [(? number?) s]
      [(? procedure?) s])))

(define (subst x v s)
  (match s
    [(== x) `',v]
    [`(if ,e1 ,e2 ,e3) `(if ,(subst x v e1)
                            ,(subst x v e2)
                            ,(subst x v e3))]
    [`(lambda (,x0) ,b)
     (if (eq? x0 x)
         s
         `(lambda (,x0)
            ,(subst x v b)))]
    [`- -]
    [`+ +]
    [(list e ...) (map (lambda (e) (subst x v e)) e)]
    [_ s]))

(module+ test
  (require rackunit)
  (check-equal? (interp '(let ([x 1]) (or x (+ x 2)))) 1)
  (check-equal? (interp '((lambda (x) (or x 2 y)) 0)) 2)
  (check-equal? (interp `(let ([x 10]) (or 0 x))) 0)
  (check-equal? (interp `(cond [1 2]
                               [0 3])) 2)
  (check-equal? (interp `(cond [0 2]
                               [else 3])) 3)
  (check-equal? (interp `(case 'x
                           [x 10]
                           [y 20])) 10)
  (check-equal? (interp `(cond
                           [(symbol=? 'y 'x) 11]
                           [(symbol=? 'y 'y) 20])) 20)

  (check-equal? (interp `(case (let ([x 'y]) x)
                           [x 11]
                           [y 20])) 20)
  (check-equal? (interp `(letrec ([sum (lambda (x)
                                         (if x (+ x (sum (- x 1)))
                                             0))])
                           (sum 10))) 55)
  (check-equal? (interp `(letrec ([fact (lambda (x)
                                         (if x (* x (fact (- x 1)))
                                             1))])
                           (fact 5))) 120)
  (check-equal? (interp `(loop sum (n (if n (+ n (sum (- n 1)))
                                          0))
                               (sum 10))) 55)
  (check-equal? (interp `(loop fact (n (if n (* n (fact (- n 1)))
                                          1))
                               (fact 5))) 120)
  (check-equal? (interp '(for/list ([x '(1 2 3)])
                           x))
                '(1 2 3))
  (check-equal? (interp '(for/list ([x '(1 2 3)])
                           (+ 1 x)))
                '(2 3 4))
  (check-equal? (interp '(for/list ([x '(1 2 3)])
                           (* x x)))
                '(1 4 9)))
