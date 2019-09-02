#lang typed/racket

(define-type Red-Blue-Tree (U leaf red-node blue-node))
(struct leaf ([num : Integer]))
(struct red-node ([left : Red-Blue-Tree] [right : Red-Blue-Tree]))
(struct blue-node ([trees : (Listof Red-Blue-Tree)]))
