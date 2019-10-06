#lang racket

(module+ test
  (require rackunit rackunit/text-ui)
  )

;; Exercise 1.1
;; The solution is in eopl3.org, here is the tests
;; 1. {3n + 2 | n in N}
(define/contract (in-3n+2? n)
  (-> natural? boolean?)
  (if (< n 2)
      #f
      (or (= n 2)
          (in-3n+2? (- n 3)))))
(module+ test
  (check-false (in-3n+2? 1))
  (check-false (in-3n+2? 4))
  (check-true (in-3n+2? 2))
  (check-true (in-3n+2? 5))
  (check-true (in-3n+2? 8))
  )

;; 2. {2n + 3m + 1 | n, m ∈ N}
(define/contract (in-2n+3m+1? n)
  (-> natural? boolean?)
  (if (< n 1)
      #f
      (or (= n 1)
          (in-2n+3m+1? (- n 2))
          (in-2n+3m+1? (- n 3))))
  )
(module+ test
  (check-false (in-2n+3m+1? 0))
  (check-false (in-2n+3m+1? 2))
  (check-true (in-2n+3m+1? 1))
  (check-true (in-2n+3m+1? 3))
  (check-true (in-2n+3m+1? 4))
  (check-true (in-2n+3m+1? 5))
  (check-true (in-2n+3m+1? 6))
  )
