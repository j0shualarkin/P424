#lang racket

;; Fred Fu and Joshua Larkin
;; yuqfu       joslarki
;; Sam Tobin-Hochstadt -- samth
;; P424 -- Assignment 1

(require rackunit
         racket/list)

;; A Graph is a [List [List Symbol]]
;; the first Symbol in the list is the domain vertice
;; the succeeding symbols are the vertices with edges
;; from the domain vertices 
(struct graph [edges])

;; Sample Graphs
(define triangle
  (graph '((A B C)
           (B A C)
           (C A B))))

(define 7verts
  (graph '((A B C D)
           (B E C)
           (C)
           (D C F)
           (E)
           (F E)
           )))


(define G2
  (graph '((A C D)
           (B E)
           (C D E)
           (D A B)
           (E))))

;; --------

;; read-file :: (U File String) -> Graph
;; converts a file of s-expression represented graph
;; to the host graph representation
(define (read-graph st)
  ;; input : String
  (define input (read st))
  (graph
   (for/list ([e input])
     (cons (car e) (cadr e)))))

;; graph=? : Graph -> Graph -> Boolean
;; checks for equality between graphs by checking for
;; equality between the list of vertices in the graphs 
(define (graph=? G1 G2)
  (equal? (graph-edges G1) (graph-edges G2)))

(check graph=? (read-graph (open-input-file "graph-test-fixtures")) triangle)
(check graph=? (read-graph (open-input-file "graph-test-fixtures2")) 7verts)

;; ----
;; Printing Graphs
;; ----

;; slice : Number -> Number -> Number -> List
;; helper function for printing,
;; allows for printing of sets of vertices that arent the first set nor the last
(define (slice lst start end)
  (take (drop lst start) (- end start)))


;; print-graph : Graph -> Void
;; prints the graph in s-exp form as desired from course webpage
(define print-graph
  (λ (G)
    (define data (graph-edges G))
    (define final (last data))

    (display "(")
    (displayln (cons (caar data) (list (cdar data))))

    (for ([i (slice data 1 (- (length data) 1))])
      (display " ")
      (displayln (cons (car i) (list (cdr i)))))

    (display " ")
    (display (cons (car final) (list (cdr final))))
    (displayln ")")))

;; spanning-tree : Graph -> Graph
;; returns a graph with the same amount of vertices, but
; edges are minimized to only the essentials needed to traverse the graph 
(define (spanning-tree G)
  (define seen `(,(caar (graph-edges G))))
  (graph (for/list ([i (graph-edges G)])
           (cons (car i)
                 (for/list ([vt (cdr i)] #:when (not (memv vt seen)))
                   (begin (set! seen (cons vt seen))
                          vt))))))

(check-equal? (graph-edges (spanning-tree triangle)) '((A B C) (B) (C)))
(check-equal? (graph-edges (spanning-tree 7verts)) '((A B C D) (B E) (C) (D F) (E) (F)))
(check-equal? (graph-edges (spanning-tree G2)) '((A C D) (B E) (C) (D B) (E)))


#| Old code, may become useful when we try
   a more racket//idiomatic approach to printing graphs

(define (graph-write graph port mode)
  (write-string "(" port)
  (for ([e (graph-edges graph)])
    (write-string (format "~a\n" (cons (car e) (list (cdr e))))))
  (write-string ")" port))


(struct graph [edges]
  #:methods gen:custom-write
  ((define write-proc graph-write)))
|#