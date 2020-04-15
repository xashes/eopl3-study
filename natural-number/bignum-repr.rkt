#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide zero
         zero?
         successor
         predecessor)

;; n = '() when n = 0
;;   | (cons r q) when n = qN + r, 0 <= r < N
(define-type Bignum (Listof Nonnegative-Integer))
(define BASE : Positive-Integer 16)

;; constructor
(: zero (-> Bignum))
(define (zero)
  null)

(: zero? (-> Bignum Boolean))
(define (zero? n)
  (null? n))

(: successor (-> Bignum Bignum))
(define (successor n)
  (if (zero? n)
      '(1)
      (let ((r (add1 (car n))))
        (if (= r BASE)
            (cons 0
                  (successor (cdr n)))
            (cons r (cdr n)))
        )))
(module+ test
  (check-equal? (successor '()) '(1))
  (check-equal? (successor '(15 15 8))
                '(0 0 9))
  )

(: predecessor (-> Bignum Bignum))
(define (predecessor n)
  (if (zero? n)
      (error 'predecessor "n should be greater than zero")
      (let ((r : Natural (car n))
            (bn : Natural (sub1 BASE)))
        (if (= r 0)
            (cons bn
                  (predecessor (cdr n)))
            (cons (sub1 r)
                  (cdr n)))))
  )
(module+ test
  (check-equal? (predecessor '(7 8 9))
                '(6 8 9))
  (check-equal? (predecessor '(0 8 9))
                '(15 7 9))
  (check-equal? (predecessor '(0 0 9))
                '(15 15 8))
  )
