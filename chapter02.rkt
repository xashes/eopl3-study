#lang typed/racket

(require "./natural-number/bignum-repr.rkt")

(module+ test
  (require typed/rackunit))

;; Exercise 2.1
;; Implement the four required operations for bigits.
;; Then use your implementation to calculate the factorial of 10.
;; How does the execution time vary as this argument changes?
;; How does the execution time vary as the base changes? Explain why.
(: factorial (-> Bignum Bignum))
(define (factorial n)
  (if (zero? n)
      '(1)
      (zero))
  )
(module+ test
  (check-equal? (factorial '(3))
                '(6))
  (check-equal? (factorial null)
                '(1))
  )
