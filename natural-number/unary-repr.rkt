#lang racket

(module+ test
  (require rackunit rackunit/text-ui)
  )

(provide natural?
         zero
         is-zero?
         successor
         predecessor)

;; Natural ::= '()
;;           | (#t . Natural)
(define/contract (natural? v)
  (-> any/c boolean?)
  (and (list? v)
           (andmap (lambda (x) (eq? x #t))
                   v)))
(module+ test
  (check-true (natural? null))
  (check-true (natural? '(#t #t)))
  (check-false (natural? '(#t 1)))
  (check-false (natural? 1))
  )
;; constructors
(define/contract (zero)
  (-> null?)
  null
  )
(module+ test
  (check-equal? (zero) null)
  )

(define/contract (successor n)
  (-> natural? natural?)
  (cons #t n)
  )
(module+ test
  (check-equal? (successor '())
                '(#t))
  )

(define/contract (predecessor n)
  (-> natural? natural?)
  (if (is-zero? n)
      n
      (cdr n))
  )
(module+ test
  (check-equal? (predecessor (zero)) (zero))
  (check-equal? (predecessor '(#t)) (zero))
  )

;; observer
(define/contract (is-zero? n)
  (-> natural? boolean?)
  (equal? n (zero))
  )
(module+ test
  (check-false (is-zero? '(#t)))
  (check-true (is-zero? '()))
  )
