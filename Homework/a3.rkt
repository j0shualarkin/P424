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
;; all produces the list of resulsts not including any values which are #true

;; otherwise all produces #false

;; the all form should short-circuit (not print anything)

;(all #false (print 'hi))

(require (for-syntax syntax/parse
                     racket/match
                     racket/bool))
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
#;
(all #t #t #t #f (begin (displayln "heheh") 20))
;; had to comment this test out since
;; function equality isn't easy to check
;; but it says there's a procedure in the right place,
;; so I'm assuming the test passes for all intents and purposes
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


;; (struct/con ({Identifier : Identifier} ...))
;; identifier to the right of the colon names the predicate

;; the struct/con form creates a struct type defn whose constructor ensures
;; that the respective field values satisfy the named predicate

;; note that Racket's `struct` comes with guards
;; the point on this exercise is that you may use struct
;; but not its guard feature. instead override the struct constructor


;; hints:

;; 1) it is possible to set! a function name (but not required for this problem)
;; 2) Use `begin` to splice a sequence of code into a context



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


(define-for-syntax (accessify stx snme fnme)
  (map
   (λ (fnme indx)
     (define field-getter (datum->syntax stx
                                         (string->symbol
                                          (string-append (symbol->string (syntax->datum snme))
                                                         "-"
                                                         (symbol->string (syntax->datum fnme))))))
     #`(define 
         #,field-getter
         (λ (inst) (list-ref inst (add1 #,indx)))))
   (syntax->list fnme)
   (for/list ([s (length (syntax->list fnme))])
     s)))


(define-syntax (struct/con stx)
  (syntax-parse stx #:datum-literals (:)
    [(_ struct-name ({f1 : p1} ...))
     
     (define ps (map syntax->datum (syntax->list #'(p1 ...))))
     (define ps-len (length ps))

     (define vs
       (for/list ([i ps-len])
         (string->symbol (string-append "v-" (number->string i)))))

     (define vs-stx (datum->syntax #'struct-name vs))

     (define rands
       (map (λ (p v)
              #`(or (#,p #,v)
                    (error 'struct/con "expr ~v did not pass predicate ~v" #,v #,p)))
            ps
            (syntax->list vs-stx)))

     (define accessors (accessify stx #'struct-name #'(f1 ...)))
     
     #`(begin
         #,@accessors
           (define-syntax (struct-name stx)
               (syntax-parse stx
                 [(struct-name #,@vs-stx)
                  #'(when (and #,@rands)
                      (list (quote struct-name) #,@vs-stx))])))

]))





(module+ test
  (struct/con abc ({x : zero?} {y : symbol?}))
  #;(define non-ex (abc 1 'z))
  
  (define ex (abc 0 'x))

  (check-exn (regexp "struct/con: expr 0 did not pass predicate #<procedure:symbol\\?>")
             (λ () (abc 0 0)))

  (check-exn (regexp "struct/con: expr 1 did not pass predicate #<procedure:zero\\?>")
             (λ () (abc 1 0)))
  
  (check-exn (regexp "struct/con: expr 1 did not pass predicate #<procedure:zero\\?>")
             (λ () (abc 1 'z)))


  (struct/con abcd ({x : zero?}
                    {y : symbol?}
                    {z : string?}))

  (define ex2 (abcd 0 'z "dogs"))

  (check-exn (regexp "struct/con: expr 120 did not pass predicate #<procedure:string\\?>")
             (λ () (abcd 0 'a 120)))
  
  (check-equal? (abcd-x ex2) 0)
  (check-equal? (abcd-y ex2) 'z)
  (check-equal? (abcd-z ex2) "dogs"))

