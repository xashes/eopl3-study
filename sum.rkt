#lang racket

;; sum : Listof(Number) -> Number
;; no need to use fold
(define (sum lst)
  (let sum-from ([n 0]
                 [lst lst])
    (if (empty? lst)
        n
        (sum-from (+ n (car lst))
                  (cdr lst)))))


(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (sum '()) 0)
  (check-equal? (sum '(1 2 3)) 6)

  )
