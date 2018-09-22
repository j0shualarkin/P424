#lang racket

(module spec racket

  (require (for-syntax syntax/parse)
           (rename-in racket (define orig-define)
                      (#%module-begin modbeg)))
  (provide ;name author university
           #;define
           (rename-out [our-define define])
           #%module-begin
           #%datum
           #%app
           #%top)

  (begin-for-syntax
    (define-syntax-class fields
      (pattern f:id #:fail-unless (member #'f (list #'author #'university #'name) free-identifier=?) "not name...")))

  (define-syntax (#%module-begin stx)
    (syntax-parse stx
      ((_ e ...) #'(modbeg
                    (provide name author university)
                    e ...))))
  
  (define-syntax (our-define stx)
    (syntax-parse stx
      [(_ id1:fields id2:id) #`(define id1 (quote #,(syntax->datum #'id2)))])
    )
#;#;#;
  (define name j)
  (define author a)
  (define university u))

;; all top level forms are (define id1 id2)
;; quotes all identifier on RHS

(module test (submod ".." spec)
  (define name Froshua)
  name
  (define author samtthias)
  author
  (define university indiana)
  university
  )
