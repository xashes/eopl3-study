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

;; List ::= '() | (any/c . List)
;; usage: (list-length l)
;; return: the length of l
(define/contract (list-length lst)
  (-> list? integer?)
  (if (empty? lst)
      0
      (add1 (list-length (rest lst)))))
(module+ test
  (check-equal? (list-length '()) 0)
  (check-equal? (list-length '(a b c)) 3)
  )

(define/contract (nth-element lst n)
  (-> list? natural? any/c)
  (let helper ([l lst] [i n])
    (cond
      [(empty? l)
       (error 'nth-element "~a does not have ~s elements" lst (add1 n))]
      [(zero? i) (first l)]
      [else (helper (rest l) (sub1 i))]))
  )
(module+ test
  (check-exn exn:fail? (lambda () (nth-element '() 0)))
  (check-equal? (nth-element '(a) 0) 'a)
  (check-equal? (nth-element '(a b c d e) 3) 'd)
  )

(define/contract (remove-first v l)
  (-> any/c list? list?)
  (if (empty? l)
      null
      (if (equal? v (first l))
          (rest l)
          (cons (first l)
                (remove-first v (rest l))))))
(module+ test
  (check-equal? (remove-first 'a '(a b c)) '(b c))
  (check-equal? (remove-first 'a '(a b a c)) '(b a c))
  (check-equal? (remove-first 'a '(1 2 b c)) '(1 2 b c))
  )

;; LcExp ::= symbol?
;;         | (lambda (symbol?) LcExp)
;;         | (LcExp LcExp)
;; cases
;; exp is a symbol. return #t if and only if (eq? s exp)
;; exp is a lambda exp. return #t if (not (eq? s (bound-var exp))) and s in (body exp)
;; exp is a app exp. return #t if s occurs free in e1 or e2

;; (define/contract (occurs-free? s exp)
;;   (-> symbol? any/c boolean?)
;;   (cond
;;     [(symbol? exp) (eq? s exp)]
;;     [(eq? (first exp) 'lambda) (and (not (eq? s (caadr exp)))
;;                                     (occurs-free? s (caddr exp)))]
;;     [else (or (occurs-free? s (car exp))
;;               (occurs-free? s (cdr exp)))])
;;   )

(define/contract (occurs-free? s exp)
  (-> symbol? any/c boolean?)
  (match exp
    [(? symbol? exp) (eq? s exp)]
    [`(lambda (,var) ,body) (and (not (eq? s var))
                                 (occurs-free? s body))]
    [(list e1 e2) (or (occurs-free? s e1)
                      (occurs-free? s e2))]
    [_ #f]
    )
  )
(module+ test
  (check-equal? (occurs-free? 'x 'x) #t)
  (check-equal? (occurs-free? 'x 'y) #f)
  (check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f)
  (check-equal? (occurs-free? 'x '(lambda (y) (x y))) #t)
  (check-equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t)
  (check-equal? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)
  )
