#lang racket


(module lab4 racket
  (require (for-syntax syntax/parse)
           (rename-in racket [+ orig-+]
                      [let-values orig-let-values]
                      ))
  (provide + *
           let-values
           #%module-begin
           #%app
           #%datum
           #%top
           define
           quote
           values
           let)
  
  (define-syntax (+ stx)
    (syntax-parse stx
      [(+ 0 n:expr) #'(if (number? n) n (error '+ "expected number, got ~v" n))]
      [(+ n:expr 0) #'(if (number? n) n (error '+ "expected number, got ~v" n))]
      [(+ m:expr n:expr) #'(orig-+ m n)]
      [_ (error "oops, expected exprs, got" (syntax->datum stx))]))


  (define-syntax (let-values stx)
    (syntax-parse stx #:literals (values)

      ((let-values ([x:id] (values e e2 ...)) y:id)
       (raise-syntax-error 'let-values "cant bind one var to multiple values" stx)
       #;#'(orig-let-values ([x] (values e e2 ...)) y))
      
      ((let-values ([x:id] e:expr) y:id) #:when (free-identifier=? #'x #'y) #'e)
      )))

;; third optimization : (* x 1) || (* 1 x)   ==> x
;; 4th optimization   : (append xs nil) || (append nil xs) ==> xs

(module test (submod ".." lab4)
  
  (let-values ([x] 5) x)
  (let-values ([x] (+ 2 3)) x)

  #;
  (let ([y (values 1 2 3)])
    (let-values ([x] y) x))
  #;(let-values ([x] (values 1 2 3)) x)
  



  ;(require rackunit)
  (+ 1 0) ;; 1
  (+ 0 2) ;; 2
  (+ (+ 2 0) (+ 0 1)) ;; 3
  (define x 5)
  (+ x 0)  ;; 5
  (+ 'h 0) ;; error

)


