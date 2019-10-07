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

;; 3. {(n, 2n + 1) | n ∈ N}
(define/contract (in-pair-set? n m)
  (-> natural? natural? boolean?)
  (if (or (< n 0)
          (< m 1))
      #f
      (or (and (zero? n) (= m 1))
          (in-pair-set? (- n 1) (- m 2)))))
(module+ test
  (check-false (in-pair-set? 0 0))
  (check-false (in-pair-set? 0 3))
  (check-true (in-pair-set? 0 1))
  (check-true (in-pair-set? 1 3))
  (check-true (in-pair-set? 2 5))
  )

;; 4. {(n, n^2) | n ∈ N}
(define/contract (in-n-nsqr? n m)
  (-> natural? natural? boolean?)
  (if (or (< n 0)
          (< m 0))
      #f
      (or (and (zero? n)
               (zero? m))
          (in-n-nsqr? (sub1 n) (add1 (- m (* n 2)))))))
(module+ test
  (check-false (in-n-nsqr? 0 1))
  (check-false (in-n-nsqr? 1 9))
  (check-true (in-n-nsqr? 0 0))
  (check-true (in-n-nsqr? 4 16))
  (check-true (in-n-nsqr? 8 64))
  )
