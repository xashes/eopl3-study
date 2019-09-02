#lang racket

;; usage: (barrel-sort loi) -> a sorted list
;; barrel-sort : List(Int) -> List(Int)
;; List(Int) ::= () | (Int . List(Int))
(define (barrel-sort loi)
  (let ([lon (range (length loi))]
        [n 0])
    (map (lambda (i)
           (count-occurrences i lon n))
         loi)))

(define (count-occurrences i lon n)
  (if (empty? lon)
      '()
      (if (= i (car lon))
          (cons (cons i (add1 n))
                (count-occurrences i (cdr lon) n))
          (cons (car lon)
                (count-occurrences i (cdr lon) n)
                ))
      ))

(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (barrel-sort '(4 3 3 2 1)) '(1 2 3 3 4))

  )
