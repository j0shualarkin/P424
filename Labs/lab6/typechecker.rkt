#lang racket

(require syntax/parse (for-syntax syntax/parse)
         (for-template racket))
(provide typecheck empty-env typed-lambda typecheck-top)

(define (type=? a b)
  (syntax-parse (list a b) #:literals (->)
    [(i:id j:id) #:when (free-identifier=? #'i #'j) #t]
    [((p at dt) (p^ at^ dt^)) #:when (and (eqv? (syntax-e #'p) 'Pair)
                                          (eqv? (syntax-e #'p^) 'Pair))
                              #:when (and
                                           (type=? #'at #'at^)
                                           (type=? #'dt #'dt^)) #t]
    [((-> dom ... rng) (-> dom* ... rng*))
     #:when (andmap type=?
                    (syntax->list #'(dom ...))
                    (syntax->list #'(dom* ...)))
     #:when (type=? #'rng #'rng*)
     #t]
    [_ #f]))

(define (typecheck-top stx env)
  (syntax-parse stx #:literals (#%plain-lambda #%plain-app + quote
                                               list let-values
                                               begin define-values
                                               letrec-values)
    [(define-values (i:id) (begin 't e:expr))
     (define t* (typecheck #'e (cons (cons #'i #'t) env)))
     (unless (type=? #'t t*)
       (raise-syntax-error #f "not the same" #'t))
     (cons (cons #'i #'t) env)]
    [_ (typecheck stx env)
       env]))

(define (typecheck stx env)
  (syntax-parse stx #:literals (#%plain-lambda #%plain-app + quote
                                               list let-values
                                               begin define-values
                                               letrec-values
                                               if car cdr cons)
    [(quote n)
     #:when (integer? (syntax-e #'n))
     ;(printf "~v\n" (syntax-e #'n))
     #'Integer]

    [(quote b)
     #:when (boolean? (syntax-e #'b))
     ;(printf "~v\n" (syntax-e #'b))
     #'Boolean]
    
    [(quote str)
     #:when (string? (syntax-e #'str))
     ;(printf "~v\n" (syntax-e #'str))
     #'String]
    
    ;; == Pairs ==
    [(#%plain-app cons a d)
     (define a-t (typecheck #'a env))
     (define d-t (typecheck #'d env))
     #`(Pair #,a-t #,d-t)]

    [(#%plain-app car pr) (syntax-parse (typecheck #'pr env)
                            [(Pair ta td) #'ta])]
    [(#%plain-app cdr pr) (syntax-parse (typecheck #'pr env)
                            [(Pair ta td) #'td])]
    

    [(let-values ([(i:id) e:expr]) body)
     (typecheck #'body (append (list (cons #'i (typecheck #'e env)))
                               env))]
    [(letrec-values ([(i:id) e:expr]) (#%plain-app list 'ty) body)
     (define e-type (typecheck #'e (append (list (cons #'i #'ty))
                                           env)))
     (unless (type=? e-type #'ty)
       (raise-syntax-error #f "letrec is bad" #'ty))
     (typecheck #'body (append (list (cons #'i #'ty))
                               env))]
    [(#%plain-lambda (i:id ...) (#%plain-app list (quote ty) ...) body)
     (define body-type
       (typecheck #'body (append (map cons
                                      (syntax->list #'(i ...))
                                      (syntax->list #'(ty ...)))
                                 env)))
     #`(-> ty ... #,body-type)]
    [i:id (lookup #'i env)]
    [(#%plain-app op:expr rands:expr ...)
     (define op-type (typecheck #'op env))
     (define rands-types (map (λ (e) (typecheck e env))
                              (syntax->list #'(rands ...))))
     (syntax-parse op-type #:literals (->)
       [(-> ~! args ... result)
        #:fail-when (not (= (length rands-types) (length (syntax->list #'(args ...))))) "wrong number of arguments"
        #:when
        (for ([r (in-list rands-types)]
              [a (in-list (syntax->list #'(args ...)))])
          (unless (type=? r a)
            (raise-syntax-error #f (format "bad application: expected ~s got ~s" r a) stx #f (list r a))))
        #'result]
       [_ (raise-syntax-error #f (format "bad application, ~s is not a function type" op-type) stx)])]
    [(begin e ...)
     (for/last ([e (syntax->list #'(e ...))])
       (typecheck e env))]
    [(if e1 e2 e3)
     (define tst (typecheck #'e1 env))
     (define thn (typecheck #'e2 env))
     (define els (typecheck #'e3 env))
     (unless (type=? #'Boolean tst)
       (raise-syntax-error #f "not a Boolean" #'e1))
     (unless (type=? thn els)
       (raise-syntax-error #f "not the same" #'e2))
     thn]
    [_ (raise-syntax-error #f "unknown form" stx)]))

;; lookup : Identifier Environment -> Type
(define (lookup id env)
  (cond [(empty? env) (error 'not-found "can't find type of identifier: ~s" id)]
        [else (if (free-identifier=? id (caar env))
                  (cdar env)
                  (lookup id (cdr env)))]))

(define empty-env (list (cons #'display #'(-> String Void))
                        (cons #'string-length #'(-> String Integer))
                        (cons #'string-append #'(-> String String String))
                        (cons #'substring #'(-> String Integer Integer String))
                        (cons #'not #'(-> Boolean Boolean))
                        (cons #'and #'(-> Boolean Boolean Boolean))
                        (cons #'or  #'(-> Boolean Boolean Boolean))
                        (cons #'+   #'(-> Integer Integer Integer))
                        (cons #'*   #'(-> Integer Integer Integer))
                        (cons #'=   #'(-> Integer Integer Boolean))
                        (cons #'-   #'(-> Integer Integer Integer))))

(define-syntax (typed-lambda stx)
  (syntax-parse stx
    [(_ ([i:id : ty] ...) body)
     #'(#%plain-lambda (i ...)
                       (list 'ty ...)
                       body)]))
#;
(typecheck (expand #'(typed-lambda ([x : Integer]) (+ x 1)))
           empty-env)
;
;((typed-lambda ([x : Integer]) (+ x 1)) 15)
;
;(typecheck (expand
;            #'(typed-lambda ([x : (-> Integer)] [f : (-> (-> Integer) Integer)]) (f x))) empty-env)
;
;(typecheck (expand #'5) empty-env)
;(typecheck (expand #'(+ 6 5)) empty-env)
;
;(typecheck (expand
;            #'((typed-lambda ([f : (-> Integer Integer Integer)]) (f 2 3)) +))
;           empty-env)
;
;(typecheck (expand #'(letrec ([f (typed-lambda ([n : Integer]) (f (+ n -1)))])
;                       (list '(-> Integer Integer))
;                       (f 10)))
;           empty-env)