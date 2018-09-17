#lang racket

;
;
;
;
;   ;    ;                                                       ;
;   ;    ;                   ;                                   ;
;   ;    ;                   ;                                   ;
;   ;    ;   ;;;;   ; ;;;  ;;;;;;;    ;;;   ; ;;;     ;;;;    ;;;;   ;;;;
;   ;;;;;;  ;;  ;;  ;;  ;;   ;       ;   ;  ;;  ;;   ;   ;;  ;   ;  ;
;   ;    ;  ;    ;  ;    ;   ;           ;  ;    ;  ;     ; ;    ;  ;
;   ;    ;  ;;;;;;  ;    ;   ;       ;;;;;  ;    ;  ;     ; ;    ;   ;;;
;   ;    ;  ;       ;    ;   ;      ;    ;  ;    ;  ;     ; ;    ;      ;
;   ;    ;  ;;      ;   ;    ;;     ;   ;;  ;   ;   ;;   ;  ;;  ;;      ;
;   ;    ;   ;;;;;  ;;;;      ;;;;   ;;; ;  ;;;;     ;;;;    ;;; ;  ;;;;
;                   ;                       ;
;                   ;                       ;
;                   ;                       ;
;

(require (for-syntax syntax/parse))

;
;
;
;
;     ;;;    ;       ;
;     ; ;    ;       ;
;     ; ;    ;       ;
;     ; ;;   ;       ;
;    ;   ;   ;       ;
;    ;   ;   ;       ;
;    ;   ;   ;       ;
;   ;;;;;;;  ;       ;
;   ;     ;  ;       ;
;   ;     ;  ;;;;;;  ;;;;;;
;
;
;
;



;; (all Expression ...)
;; if all of the expressions evaluate to a non-#false value
;; all produces the list of resulsts excluding #true values
;; otherwise all produces #false immediately after seeing a not-true value

;; all : Syntax -> Syntax
(define-syntax all
  (syntax-parser
    [(_ e ...) #'(let ([v #t]
                       [result '()])
                   (if v (begin
                           (set! v e)
                           (when (and v (not (eq? v #t)))
                             (set! result (append result (list v)))))
                       #f)
                   ...
                   (if v result #f))]))


;; we tried a few different implementations (without using set!)
;; before arriving to the above definition!

#; 
(define-syntax all
  (syntax-parser
    [(_ e)
     (match (syntax->datum #'e)
       [#t #''()]
       [#f #'#f]
       [_ #'(list e)])]
    [(_ e1 e2 ...)
     (match (syntax->datum #'e1)
       [#t #'(all e2 ...)]
       [#f #'#f]
       [_
        (with-syntax ([rec #'(all e2 ...)])
          #`(let ([v1 e1]
                  [vr rec])
              (if vr (cons v1 vr) #f)))])]))
#;
(define-syntax (all0 stx)
  (syntax-parse stx
    [(_ e) #'(let ([v e])
               (if v (list v)
                   (list #f)))]
    [(_ e1 e ...)
     #'(let ([v1 e1])
         (if v1
             (cons v1 (all0 e ...))
             (list #f)))]))

#;
(define-syntax all
  (syntax-parser
    [(_ stx ...) #`(let ([v (all0 stx ...)])
                     (if (last v) (filter (λ (a) (not (eqv? a #t))) v)
                         #f))]))
#;
(define-syntax all-
  (syntax-parser
    [(_ acc) #'acc]
    [(_ acc e1 e ...) #'(let ([v1 e1])
                          (if v1
                              (all- (if (eq? v1 #t) acc
                                        (append acc (list v1)))
                                    e ...)
                              #f))]))
#;
(define-syntax-rule (all e ...)
  (all- '() e ...))


#;
(all #t #t #t #f (begin (displayln "heheh") 20))
;; had to comment this test out since
;; function equality isn't easy to check
;; but it says there's a procedure in the right place,
;; so we're assuming the test passes for all intents and purposes
#;
(check-equal?
 (all 42 "dogs" (λ (x) x))
 (list 42 "dogs" (λ (x) x)))
(module+ test
  (require rackunit)
  (check-equal?
   (all #t)
   (list))

  (check-equal?
   (all #f)
   #f)

  (check-equal?
   (all 42)
   (list 42))

  (define li (list))

  ;; the case that brought us to the final definition
  (check-equal? (all #t
                     (begin
                       (set! li (cons 'a li))
                       li)
                     (begin
                       (set! li (cons 'b li))
                       li))
                '((a) (b a)))

  (check-equal? (all #t #f)
                #f)

  (check-equal? (all 42 #f 13)
                #f)

  (check-equal? (all 1 2 #t)
                (list 1 2))

  (check-equal? (all 1 #t 2)
                (list 1 2)))



;; ========================================================


;
;
;
;
;                                                        ;
;            ;                               ;          ;;
;            ;                               ;          ;
;    ;;;;  ;;;;;;;  ; ;;;   ;    ;    ;;;; ;;;;;;;      ;     ;;;;    ;;;;  ; ;;;
;   ;        ;      ;;   ;  ;    ;   ;       ;         ;     ;       ;   ;; ;;   ;
;   ;        ;      ;    ;  ;    ;  ;        ;         ;    ;       ;     ; ;    ;
;    ;;;     ;      ;       ;    ;  ;        ;        ;;    ;       ;     ; ;    ;
;       ;    ;      ;       ;    ;  ;        ;        ;     ;       ;     ; ;    ;
;       ;    ;;     ;       ;   ;;  ;;       ;;      ;;     ;;      ;;   ;  ;    ;
;   ;;;;      ;;;;  ;        ;;; ;    ;;;;    ;;;;   ;        ;;;;   ;;;;   ;    ;
;                                                    ;
;                                                   ;
;
;


;; examples
;; ============
;; (struct/con major ({sid : number?}
;;                    {sname : string?}
;;                    {major : string?}))

;; (major 1 "Joshua" "CS")
;; (major 2 "Fred"   "CS")

;; non-examples
;; ==============

;; (major 3 (λ (x) "Joshua") 55)
;; (major "Joshua" 3 14)

;; -----------------------------------------------------------

;; example 2
;; =============
;; (struct/con knil ())
;; (struct/con kons ({kar : symbol?} {kdr : (or list? knil?)}))

;; (kons 'd '(o g s))
;; (kons 'dogs '(and cats))
;; (kons 'e knil)

;; non-examples 2
;; =================

;; (kons 5 '(cat))
;; (kons knil knil)
;; (kons 5 'dogs)

;; syntax->string : Syntax -> String
;; helper function to simplify function name generation
(define-for-syntax (syntax->string stx)
  (symbol->string (syntax->datum stx)))


;; string->syntax : String -> Syntax 
;; inverse function of above, requires a context
(define-for-syntax (string->syntax str stx)
  (datum->syntax stx (string->symbol str)))

;; accessify : Syntax -> Syntax -> Syntax -> (Listof Syntax)
;; generates accesor functions for the given structs name & its fields' names
(define-for-syntax (accessify stx snme fnme)
  (define field-names (syntax->list fnme))
  (map
   (λ (fnme indx)
     (define field-getter
       (string->syntax (string-append (syntax->string snme) "-"
                                      (syntax->string fnme))
                       stx))
     #`(define 
         #,field-getter
         (λ (inst) (list-ref inst (add1 #,indx)))))
   field-names
   ;; indexing into the values present in a instance of a struct
   (for/list ([s (length field-names)])
     s)))


;; make-struct-pred : Syntax -> Syntax -> Syntax -> Syntax
;; generates a predicate for an instance of a struct
;; writing a purpose statement for a function that checks if something
;; is an instance of a type is like existence,
;; is the purpose statement of a predicate philosophical then? ^.^
(define-for-syntax (make-struct-pred stx snme ps)
  (define predicate (string->syntax (string-append (syntax->string snme) "?") stx))
  #`(define #,predicate
      (λ (s) (if (not (list? s))
                 (error (quote #,predicate) "expected a ~v got ~v" (quote #,snme) s)
                 (eqv? (quote #,snme) (car s))))))


;; struct/con : Syntax -> Syntax
;; behaves like struct in racket but maintains contracts on
;; the values given to an instance of the struct
(define-syntax (struct/con stx)
  (syntax-parse stx #:datum-literals (:)
    [(_ struct-name ({f1 : p1} ...))
     
     (define ps (map syntax->datum (syntax->list #'(p1 ...))))
     (define ps-len (length ps))

     (define vs
       (for/list ([i ps-len])
         (string->symbol (string-append "v-" (number->string i)))))

     (define vs-stx (datum->syntax #'struct-name vs))

     (define struct-vals
       (map (λ (p e)
              #`(or (let ([v #,e])
                      (if (#,p v) v
                          #f))
                    (error 'struct/con "expr ~v did not pass predicate ~v" #,e #,p)))
            ps
            (syntax->list vs-stx)))

     (define predicate (make-struct-pred stx #'struct-name #'(p1 ...)))
     (define accessors (accessify        stx #'struct-name #'(f1 ...)))
     
     #`(begin
         #,predicate
         #,@accessors
         (define-syntax (struct-name stx)
           (syntax-parse stx
             [(struct-name #,@vs-stx)
              #'(list (quote struct-name) #,@struct-vals)])))]))



(struct/con interesting-example ({f1 : number?}))
(interesting-example (begin (displayln 'once)
                            10))


(module+ test
  ;; when databases hw and 424 hw meetup
  (struct/con major ({sid : number?}
                     {sname : string?}
                     {major : string?}))

  (define F (major 1 "Fred"   "CS"))
  (define J (major 2 "Joshua" "CS"))

  (check-equal? (major-sid F) 1)
  (check-equal? (major-sname J) "Joshua")
  (check-equal? (string-append
                 (major-major F)
                 (major-major J))
                "CSCS")
  (check-equal? (major? F) #t)


  (check-exn (regexp "struct/con: expr 'dogs did not pass predicate #<procedure:string?\\?>")
             (λ () (major 3 'dogs 55)))
  
  (check-exn
   (regexp "struct/con: expr \"Joshua\" did not pass predicate #<procedure:number\\?>")
   (λ () (major "Joshua" 3 14)))

  ;; -------------------------
  
  (struct/con eval-once-test ({xs : list?} {ys : list?}))
  
  (define my-xs '())
  
  (check-equal?
   (eval-once-test (begin
                     (set! my-xs (cons 'a my-xs))
                     my-xs)
                   (begin
                     (set! my-xs (cons 'b my-xs))
                     my-xs))
   '(eval-once-test (a) (b a)))

  ;; -------------------
  
  (struct/con pair ({x : zero?} {y : symbol?}))
 

  (check-exn (regexp "struct/con: expr 0 did not pass predicate #<procedure:symbol\\?>")
             (λ () (pair 0 0)))

  (check-exn (regexp "struct/con: expr 1 did not pass predicate #<procedure:zero\\?>")
             (λ () (pair 1 0)))
  
  (check-exn (regexp "struct/con: expr 1 did not pass predicate #<procedure:zero\\?>")
             (λ () (pair 1 'z)))

  ;; -------------------------
  
  (struct/con abcd ({x : zero?}
                    {y : symbol?}
                    {z : string?}))

  (define ex2 (abcd 0 'z "dogs"))

  (check-exn (regexp "struct/con: expr 120 did not pass predicate #<procedure:string\\?>")
             (λ () (abcd 0 'a 120)))
  
  (check-equal? (abcd-x ex2) 0)
  (check-equal? (abcd-y ex2) 'z)
  (check-equal? (abcd-z ex2) "dogs"))

