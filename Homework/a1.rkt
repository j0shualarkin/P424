#lang racket

(require rackunit
         (for-syntax syntax/parse)
         2htdp/batch-io
         graph)


#;(define (graph-write graph port mode)
  (write-string "(" port)
  (for ([e (graph-edges graph)])
    (write-string (format "~a\n" (cons (car e) (list (cdr e))))))
  (write-string ")" port))
#;
(struct graph [edges]
  #:methods gen:custom-write
  ((define write-proc graph-write)))

(struct graph [edges])

(define (slice lst start end)
  (take (drop lst start) (- end start)))

(require racket/list)



(define triangle (graph '((A B C)
                          (B A C)
                          (C A B))))

(define 7verts (graph '((A B C D)
                          (B E C)
                          (C)
                          (D C F)
                          (E)
                          (F E)
                          )))

;; read-file :: (U File String) -> Graph
;; converts a file of s-expression represented graph
;; to the host graph representation
(define (read-graph st)
  ;; input : String
  (define input (read st))
  (graph
   (for/list ([e input])
     (cons (car e) (cadr e)))))


(define (graph=? G1 G2)
  (equal? (graph-edges G1) (graph-edges G2)))

(check graph=? (read-graph (open-input-file "graph-test-fixtures")) triangle)
(check graph=? (read-graph (open-input-file "graph-test-fixtures2")) 7verts)

;; print-graph : Graph -> Void
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


(define (spanning-tree G)
  (define seen `(,(caar (graph-edges G))))
  (graph (for/list ([i (graph-edges G)])
           (cons (car i)
                 (for/list ([vt (cdr i)] #:when (not (memv vt seen)))
                   (begin (set! seen (cons vt seen))
                          vt))))))

(define G2 (graph '((A C D)
                    (B E)
                    (C D E)
                    (D A B)
                    (E))))

(check-equal? (graph-edges (spanning-tree 7verts)) '((A B C D) (B E) (C) (D F) (E) (F)))
(check-equal? (graph-edges (spanning-tree G2)) '((A C D) (B E) (C) (D B) (E)))
