#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide Bignum
         zero
         is-zero?
         successor
         predecessor
         multiply)

;; n = '() when n = 0
;;   | (cons r q) when n = qN + r, 0 <= r < N
(define-type Bignum (Listof Nonnegative-Integer))
(define BASE : Positive-Integer 16)

;; constructor
(: zero (-> Bignum))
(define (zero)
  null)

(: is-zero? (-> Bignum Boolean))
(define (is-zero? n)
  (or (null? n)
      (andmap zero? n)))

(module+ test
  (check-true (is-zero? '(0 0 0)))
  )

(: successor (-> Bignum Bignum))
(define (successor n)
  (if (is-zero? n)
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
  (if (is-zero? n)
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

(: add (-> Bignum Bignum Bignum))
(define (add x y)
  (if (is-zero? x)
      y
      (add (predecessor x)
           (successor y))))
(module+ test
  (check-equal? (add '(1 2 3) '(4 5 6))
                '(5 7 9))
  )

(: multiply (-> Bignum Bignum Bignum))
(define (multiply x y)
  (cond
    ((or (is-zero? x) (is-zero? y)) (zero))
    ((equal? x '(1)) y)
    (else
     (add y
          (multiply (predecessor x) y)))
    )
  )
(module+ test
  (check-equal? (multiply '(1 2 3) '(2))
                '(2 4 6))
  )
