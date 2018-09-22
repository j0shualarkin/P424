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

  (define-for-syntax seen (make-hash '((name . #f) (author . #f) (university . #f))))
  (define-syntax (do-module-begin stx)
    (syntax-parse stx
      [(_)
       (for ([(k v) seen])
         (unless v
           (error (format "~a isn't defined" k))))
       #'(void)]
      [(_ e1 e ...) #'(begin e1 (do-module-begin e ...))]))

  (define-syntax-rule (#%module-begin e ...)
    (modbeg (do-module-begin e ...)))

  (define-syntax (our-define stx)
    (syntax-parse stx
      [(_ id1:fields id2:id)
       (hash-set! seen (syntax->datum #'id1) #t)
       #`(define id1 (quote #,(syntax->datum #'id2)))])
    ))

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

#;
(module test (submod ".." spec)
  (define name Froshua)
  name
  (define author samtthias)
  author
  )
