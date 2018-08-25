#lang racket

(require rackunit
         (for-syntax syntax/parse)
         2htdp/batch-io)


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

(slice '(1 2 3) 1 (- (length '(1 2 3)) 1))

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

(define test1
  '((A (B C))
    (B (A C))
    (C (A B))))

#;
(define hmm
  '((A (B C))
    (B (A C))
    (C (A B))))

(define test2
  '((A (B C D))
    (B (E C))
    (C ())
    (D (C F))
    (E ())
    (F (E))))

;; read-file :: (U File String) -> Graph
;; converts a file of s-expression represented graph
;; to the host graph representation 
(define (read-graph st)  
  ;; input : String
  (define input (if (input-port? st)
                    (read st)
                    st))
  (graph
   (for/list ([e input])
     (cons (car e) (cadr e)))))


(define (graph=? G1 G2)
  (equal? (graph-edges G1) (graph-edges G2)))

(check graph=? (read-graph test1) triangle)
(check graph=? (read-graph test2) 7verts)


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

(print-graph 7verts)


(define (foo xss)
  (for/list ([xs xss])
        (for/list ([ys (cdr xs)])
           (cons (car xs) (list ys)))))

(check-equal?
 (foo '((A B C D) (B E C)))
'((A B)(A C) (A D) (B E) (B C))
)

(require graph)

(define (spanning-tree G)
  (mst-kruskal (directed-graph (graph-edges G))))
#;
(spanning-tree 7verts)


