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

;; graph=? : Graph -> Graph -> Boolean
;; checks for equality between graphs by checking for
;; equality between the list of vertices in the graphs
(define (graph=? G1 G2)
  (equal? (graph-edges G1) (graph-edges G2)))

(check-false (graph=? (graph '((A B C)
                               (B)
                               (C)))
                      (graph '((A B C)
                               (B C)
                               (C)))))

(check-true (graph=? (graph '((A B C)
                              (B C)
                              (C)))
                     (graph '((A B C)
                              (B C)
                              (C)))))


;; read-file :: (U File String) -> Graph
;; converts a file of s-expression represented graph
;; to the host graph representation
(define (read-graph st)
  (graph
   (for/list ([e (read st)])
     (cons (car e) (cadr e)))))

;; ----
;; Printing Graphs
;; ----

;; print-graph : Graph -> Void
;; prints the graph in s-exp form as desired from course webpage
(define print-graph
  (λ (G)
    (define data (graph-edges G))
    (parameterize ([pretty-print-columns 20])
      (pretty-print
       (for/list ([i data])
         (cons (car i) (list (cdr i))))))))

;; spanning-tree : Graph -> Graph
;; returns a graph with the same amount of vertices, but
; edges are minimized to only the essentials needed to traverse the graph
(define (spanning-tree G)
  (define seen (list (caar (graph-edges G))))
  (graph (for/list ([i (graph-edges G)])
           (cons (car i)
                 (for/list ([vt (cdr i)] #:when (not (memv vt seen)))
                   (begin (set! seen (cons vt seen))
                          vt))))))


(module+ test
  (check graph=? (read-graph (open-input-file "graph-test-fixtures")) triangle)
  (check graph=? (read-graph (open-input-file "graph-test-fixtures2")) 7verts)
  (check-equal? (graph-edges (spanning-tree triangle)) '((A B C) (B) (C)))
  (check-equal? (graph-edges (spanning-tree 7verts)) '((A B C D) (B E) (C) (D F) (E) (F)))
  (check-equal? (graph-edges (spanning-tree G2)) '((A C D) (B E) (C) (D B) (E)))
  (check-equal? (with-output-to-string
                (λ _
                  (print-graph 7verts)))
              "'((A (B C D))\n  (B (E C))\n  (C ())\n  (D (C F))\n  (E ())\n  (F (E)))\n"))
