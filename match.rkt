#lang racket

;; tree-sum : Binary-tree -> Number
;; Binary-tree ::= Number | (Binary-tree Binary-tree)
(define (tree-sum btree)
  (match btree
    [(? number? x) x]
    [(list e1 e2)
     (let ([v1 (tree-sum e1)]
           [v2 (tree-sum e2)])
       (+ v1 v2))]
    ))

;; calculator : Tree -> Number
;; Tree ::= Number | (Symbol . Listof(Tree))
(define (calc tree)
  (match tree
    [(? number? x) x]
    [`(,op ,e1 ,e2)
     (let ([v1 (calc e1)]
           [v2 (calc e2)])
       (match op
         ['+ (+ v1 v2)]
         ['- (- v1 v2)]
         ['* (* v1 v2)]
         ['/ (/ v1 v2)]))]))

(match '(1 . 2)
  [(list x y) 'list]
  ['(1 . 2) 'x]
  [_ 'else])

(match '(1 1 2)
  [(list x ...) x]
  [_ 'else])
(struct shoe (size color) #:transparent)
(struct hat (size style) #:transparent)
(match (list (hat 23 'bowler) (shoe 35 'red) (hat 22 'pork-pie))
  [(list (hat y z) (shoe x j) k) y])

(match `{with {x 1} {+ x 1}}
  [`{with {,id ,rhs} ,body}
   `{{lambda {,id} ,body} ,rhs}])

(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (tree-sum '(1 (2 3))) 6)

  (let ([tree '(* (+ 1 2) (* 2 6))])
    (check-equal? (calc tree) 36))

  )
