#lang typed/racket

(define-type Bintree (U leaf root node))
(struct leaf ([parent : Bintree]) #:transparent)
(struct root ([label : Integer] [left : Bintree] [right : Bintree]) #:transparent)
(struct node ([label : Integer] [left : Bintree] [right : Bintree] [parent : Bintree]) #:transparent)

(: number->bintree (-> Integer Bintree))
(define (number->bintree n)
  (node n null null null))

(: current-element (-> Bintree (U Null Integer)))
(define (current-element bt)
  (if (leaf? bt)
      null
      (node-label bt)))

(: insert-to-left (-> Integer Bintree Bintree))
(define (insert-to-left n bt)
  (if (leaf? bt)
      (node n (leaf bt) (leaf bt) bt)
      (let ([label (node-label bt)]
            [left (node-left bt)]
            [right (node-right bt)]
            [parent (node-parent bt)])
        (node label (node n left null bt) right parent))))

(: insert-to-right (-> Integer Bintree Bintree))
(define (insert-to-right n bt)
  (if (leaf? bt)
      (node n (leaf bt) (leaf bt) bt)
      (let ([label (node-label bt)]
            [left (node-left bt)]
            [right (node-right bt)]
            [parent (node-parent bt)])
        (node label left (node n right null bt) parent))))

(: move-to-left-son (-> Bintree Bintree))
(define (move-to-left-son bt)
  (if (leaf? bt)
      (error 'move-to-left-son "Already at leaf")
      (node-left bt)))

(: move-to-right-son (-> Bintree Bintree))
(define (move-to-right-son bt)
  (if (leaf? bt)
      (error 'move-to-right-son "Already at leaf")
      (node-right bt)))

(: at-root? (-> Bintree Boolean))
(define (at-root? bt)
  (if (leaf? bt)
      #f
      (empty? (node-parent bt))))

(: move-up (-> Bintree Bintree))
(define (move-up bt)
  (if (leaf? bt)
      (leaf-parent bt)
      (node-parent bt)))


(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([n1 (number->bintree 8)]
         [n2 (insert-to-left 6 n1)]
         [n3 (insert-to-right 9 n2)])
    (check-equal? (current-element n2) 8)
    )

  )

