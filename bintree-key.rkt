#lang typed/racket

(define-type Bintree (U leaf node))
(struct leaf ([num : Integer]) #:transparent)
(struct node ([key : Symbol] [left : Bintree] [right : Bintree]) #:transparent)

(: bintree->list (-> Bintree (Listof Any)))
(define (bintree->list bt)
  (if (leaf? bt)
      `(leaf ,(leaf-num bt))
      `(node ,(node-key bt)
             ,(bintree->list (node-left bt))
             ,(bintree->list (node-right bt)))))

; TODO
;; (: max-node (-> Bintree Symbol))
;; (define (max-node bt)
;;   (match bt
;;     [(node key left right)])
;;   )

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([bt (node 'a (leaf 3) (leaf 4))]
         [bt2 (node 'b (leaf -1) bt)]
         [bt3 (node 'c bt2 (leaf 1))])
    (check-equal? (bintree->list bt)
                  '(node
                    a
                    (leaf 3)
                    (leaf 4)))
    (displayln bt2)
    (displayln bt3)
    )

  )
