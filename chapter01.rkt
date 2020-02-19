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

;; S-list ::= '()
;;          | (S-exp . S-list)
;; S-exp ::= Symbol
;;         | S-list
(define/contract (subst-sexp new old se)
  (-> symbol? symbol? any/c any/c)
  (if (symbol? se)
      (if (eq? old se)
          new
          se)
      (subst new old se)))
(define/contract (subst new old slist)
  (-> symbol? symbol? list? list?)
  (if (empty? slist)
      null
      (cons (subst-sexp new old (car slist))
            (subst new old (cdr slist)))))
(module+ test
  (check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
  )
;; S-list ::= ({S-exp}*)
;; S-exp ::= Symbol | S-list
(define/contract (subst-map new old slist)
  (-> symbol? symbol? list? list?)
  (map (lambda (se) ((lambda (new old se)
                       (if (symbol? se)
                           (if (eq? old se) new se)
                           (subst-map new old se))
                       ) new old se))
       slist)
  )
(module+ test
  (check-equal? (subst-map 'a 'b '((b c) (b () d))) '((a c) (a () d)))
  )

;; 1.3 Auxiliary Procedures and Context Arguments

(define/contract (number-elements lst)
  (-> list? (listof (list/c number? any/c)))
  (let number-elements-from ([i 0]
                             [l lst])
    (if (null? l)
        null
        (cons `(,i ,(car l))
              (number-elements-from (add1 i) (cdr l)))))
  ;; (for/list ([i (in-range (length lst))]
  ;;            [a (in-list lst)])
  ;;   `(,i ,a))
  )
(module+ test
  (check-equal? (number-elements '(a b)) '((0 a) (1 b)))
  )

;; 0 <= n < (vector-length v)
(define/contract (partial-vector-sum v n)
  (-> vector? natural? natural?)
  (if (zero? n)
      (vector-ref v 0)
      (+ (vector-ref v n)
         (partial-vector-sum v (sub1 n))))
  )
(module+ test
  (check-equal? (partial-vector-sum #(1 2 3 4) 0) 1)
  (check-equal? (partial-vector-sum #(1 2 3 4) 3) 10)
  )

(define/contract (vector-sum v)
  (-> vector? natural?)
  ;; (let ([n (vector-length v)])
  ;;   (if (zero? n)
  ;;       0
  ;;       (partial-vector-sum v (sub1 n))))
  (for/fold ([total 0])
            ([i (in-vector v)])
    (+ total i))
  )
(module+ test
  (check-equal? (vector-sum #(1 2 3 4)) 10)
  )

;; Exercises

;; 1.15
;; returns a list containing n copies of x
(define/contract (duple n x)
  (-> natural? any/c list?)
  (if (zero? n)
      null
      (cons x
            (duple (sub1 n) x))))
(module+ test
  (check-equal? (duple 2 3)
                '(3 3))
  (check-equal? (duple 4 '(ha ha))
                '((ha ha) (ha ha) (ha ha) (ha ha)))
  (check-equal? (duple 0 '(blah)) '())
  )

;; 1.16
;; (invert lst)
;; where lst is a list of 2-lists (lists of length two), returns a list with each 2-list reversed.
(define/contract (invert lst)
  (-> (listof (list/c any/c any/c))
      (listof (list/c any/c any/c)))
  (if (empty? lst)
      null
      (let-values ([(fir sec) (vector->values (list->vector (car lst)))])
        (cons `(,sec ,fir)
              (invert (cdr lst)))))
  ;; (for/list ([lil (in-list lst)])
  ;;   (let-values ([(fir sec) (vector->values (list->vector lil))])
  ;;     (list sec fir)))
)
(module+ test
  (check-equal? (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))
  )

;; 1.17
;; (down lst)
;; wraps parentheses around each top-level element of lst.
(define/contract (down lst)
  (-> list? list?)
  ;; (if (null? lst)
  ;;     null
  ;;     (cons (list (car lst))
  ;;           (down (cdr lst))))
  (map list lst)
  )
(module+ test
  (check-equal? (down '(1 2 3))
                '((1) (2) (3)))
  (check-equal? (down '((a) (fine) (idea)))
                '(((a)) ((fine)) ((idea))))
  (check-equal? (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))
  )
