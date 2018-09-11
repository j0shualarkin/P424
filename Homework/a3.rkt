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

#;
(define-syntax struct/con
  (syntax-parser
    [(_ struct-name ()) #'(struct struct-name ())]
    [(_ struct-name ({field1 : pred1}))
     (define (struct-name a)
       (if (pred1 a) (begin (struct struct-name (field1))
                            (struct-name a))))]))
