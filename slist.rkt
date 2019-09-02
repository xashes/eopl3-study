#lang typed/racket

;; S-list ::= () | (S-exp . S-list)
;; S-exp ::= Symbol | S-list
(define-type S-list (U Null (Listof S-exp)))
(define-type S-exp (U Symbol S-list))
