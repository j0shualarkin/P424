#lang racket

(require rackunit
         #;(for-syntax syntax/parse)
         2htdp/batch-io)

(struct graph [edges])



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

(define test1 '((A (B C))
                (B (A C))
                (C (A B))))

(define test2 '((A (B C D))
                (B (E C))
                (C ())
                (D (C F))
                (E ())
                (F (E))))

;; read-file :: File -> Graph
;; converts a file of s-expression represented graph
;; to the host graph representation 
(define (read-graph file)
  ;; input : String
  (define input (read-file file))
  file)


#;(define-syntax (read-graph^ stx)
  (syntax-parse stx
    ((_ (v0 ())) #'(v0))
    ((_ (v0 (v1 ...))) #'(v0 v1 ...))
    ((_ (v0 (v1 ...))
        ...) #`(graph ((v0 v1 ...)
                       #,(read-graph^ #'...))))))


(check-equal? (read-graph test1) triangle)
(check-equal? (read-graph test2) 7verts)





