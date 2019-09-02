#lang racket

;; Exercise 2.1
(define N 16)

(define (zero)
  '())

(define (is-zero? n)
  (zero? (decimal-value n)))

(define (decimal-value n)
  (decimal-value-from n 0))

(define (decimal-value-from n index)
  (if (empty? n)
      0
      (+ (* (car n) (expt N index))
         (decimal-value-from (cdr n) (add1 index)))
      ))

;; List(bigit) ::= () | (bigit . List(bigit))
(define (successor n)
  (cond
    [(is-zero? n) '(1)]
    [(< (car n) (- N 1)) (cons (add1 (car n)) (cdr n))]
    [else (cons 0 (successor (cdr n)))]
    ))

(define (predecessor n)
  (if (zero? (car n))
      (cons (sub1 N) (predecessor (cdr n)))
      (cons (sub1 (car n)) (cdr n))))

(define (plus x y)
  (if (is-zero? x)
      y
      (plus (predecessor x) (successor y))))

(define (multiply x y)
  (if (is-zero? (predecessor y))
      x
      (plus x (multiply x (predecessor y)))))

(define (factorial n)
  (if (is-zero? (predecessor n))
      '(1)
      (multiply n (factorial (predecessor n))))
  )

(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (zero) '())

  (check-equal? (decimal-value '(0 1)) 16)
  (check-equal? (decimal-value '(0)) 0)
  (check-equal? (decimal-value '()) 0)
  (check-equal? (decimal-value '(15 15)) (+ 15 (* 15 16)))

  (check-equal? (is-zero? '()) #t)
  (check-equal? (is-zero? '(0)) #t)
  (check-equal? (is-zero? '(0 0)) #t)

  (check-equal? (successor '()) '(1))
  (check-equal? (successor '()) '(1))
  (check-equal? (successor '(0 1)) '(1 1))
  (check-equal? (successor '(15 15)) '(0 0 1))

  (check-equal? (predecessor '(0 0 1)) '(15 15 0))
  (check-equal? (predecessor '(0 1 1)) '(15 0 1))
  (check-equal? (decimal-value (predecessor '(0 0 0 1))) #xFFF)
  (check-equal? (decimal-value (predecessor '(1))) #x0)

  (check-equal? (decimal-value (plus '(8 9) '(8 9)))
                (+ #x98 #x98))
  (check-equal? (decimal-value (plus '(8 9 15) '(8 9 1)))
                (+ #xF98 #x198))

  (check-equal? (decimal-value (multiply '(15 15) '(0 0 15 1)))
                (* #xFF #x1F00))

  (check-equal? (decimal-value (factorial '(1))) 1)
  (check-equal? (decimal-value (factorial '(6))) 720)
  )
