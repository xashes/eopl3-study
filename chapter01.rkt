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
;; Exercise 1.18
;; (swapper s1 s2 slist)
;; returns a list the same as slist,
;; but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define/contract (swapper s1 s2 slist)
  (-> symbol? symbol? list? list?)
  (if (null? slist)
      null
      (let ([lcar (car slist)]
            [lcdr (cdr slist)])
        (if (symbol? lcar)
            (cond
              [(eq? s1 lcar) (cons s2 (swapper s1 s2 lcdr))]
              [(eq? s2 lcar) (cons s1 (swapper s1 s2 lcdr))]
              [else (cons lcar (swapper s1 s2 lcdr))])
            (cons (swapper s1 s2 lcar)
                  (swapper s1 s2 lcdr)))))
  )
(module+ test
  (check-equal? (swapper 'a 'd '(a b c d))
                '(d b c a))
  (check-equal? (swapper 'a 'd '(a d () c d))
                '(d a () c a))
  (check-equal? (swapper 'x 'y '((x) y (z (x))))
                '((y) x (z (y))))
  )
;; Exercise1.19
;; (list-set lst n x)
;; returns a list like lst,except that the n-th element, using zero-based indexing, is x.
(define/contract (list-set lst n x)
  (-> list? natural? any/c list?)
  (cond
    [(null? lst) null]
    [(zero? n) (cons x (cdr lst))]
    [else (cons (car lst)
                (list-set (cdr lst) (sub1 n) x))])
  )
(module+ test
  (check-equal? (list-set '(a b c d) 2 '(1 2))
                '(a b (1 2) d))
  (check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))
  )
;; Exercise 1.20
;; (count-occurrences s slist)
;; returns the number of occurrences of s in slist.
(define/contract (count-occurrences s slist)
  (-> symbol? list? natural?)
  (if (null? slist)
      0
      (if (list? (car slist))
          (+ (count-occurrences s (car slist))
             (count-occurrences s (cdr slist)))
          (if (eq? s (car slist))
              (add1 (count-occurrences s (cdr slist)))
              (count-occurrences s (cdr slist)))))
  )
(module+ test
  (check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
  (check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
  (check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)
  )
;; Exercise 1.21
;; (product sos1 sos2)
;; where sos1 and sos2 are each a list of symbols without repetitions, returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
(define/contract (product sos1 sos2)
  (-> (listof symbol?) (listof symbol?) (listof (list/c symbol? symbol?)))
  (if (null? sos1)
      null
      (append (product/symbol (car sos1) sos2)
              (product (cdr sos1) sos2)))
  )
(module+ test
  (check-equal? (product '(a b c) '(x y))
                '((a x) (a y) (b x) (b y) (c x) (c y)))
  )
(define/contract (product/symbol s los)
  (-> symbol? (listof symbol?) (listof (list/c symbol? symbol?)))
  (if (null? los)
      null
      (cons (list s (car los))
            (product/symbol s (cdr los)))
      )
  )
(module+ test
  (check-equal? (product/symbol 'x '(a b c))
                '((x a) (x b) (x c)))
  )
;; Exercise 1.22
;; (filter-in pred lst)
;; returns the list of those elements in lst that satisfy the predicate pred.
(define/contract (filter-in pred lst)
  (-> procedure? list? list?)
  (if (null? lst)
      null
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst))))
  )
(module+ test
  (check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
  (check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
  )
;; Exercise 1.23
;; (list-index pred lst)
;; returns the 0-based position of the first element of lst that satisfies the predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.
(define/contract (list-index pred lst)
  (-> procedure? list? (or/c natural? #f))
  (let list-index-from ([p pred]
                        [l lst]
                        [n 0])
    (if (null? l)
        #f
        (if (pred (car l))
            n
            (list-index-from p (cdr l) (add1 n)))))
  )
(module+ test
  (check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
  (check-equal? (list-index symbol? '(a (b c) 17 foo)) 0)
  (check-equal? (list-index symbol? '(1 2 (a b) 3)) #f)
  )
;; Exercise 1.24
;; (every? pred lst)
;; returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
(define/contract (every? pred lst)
  (-> procedure? list? boolean?)
  ;; (if (empty? lst)
  ;;     #t
  ;;     (and (pred (car lst))
  ;;          (every? pred (cdr lst))))
  (for/and ([a lst])
    (pred a))
  )
(module+ test
  (check-false (every? number? '(a b c 3 e)))
  (check-true (every? number? '(1 2 3 5 4)))
  )
;; Exercise1.25
;; (exists? pred lst)
;; returns #t if any element of lst satisfies pred, and returns #f otherwise.
(define/contract (exists? pred lst)
  (-> procedure? list? boolean?)
  ;; (if (empty? lst)
  ;;     #f
  ;;     (or (pred (car lst))
  ;;         (exists? pred (cdr lst))))
  (for/or ([a lst])
    (pred a))
  )
(module+ test
  (check-true (exists? number? '(a b c 3 e)))
  (check-false (exists? number? '(a b c d e)))
  )

;; Exercise 1.26
(define/contract (up lst)
  (-> list? list?)
  (if (null? lst)
      null
      (let ([c (car lst)]
            [cd (cdr lst)])
        (if (list? c)
            (append c (up cd))
            (cons c (up cd)))))
  )
(module+ test
  (check-equal? (up '((1 2) (3 4)))
                '(1 2 3 4))
  (check-equal? (up '((x (y)) () z))
                '(x (y) z))
  )

;; Exercise 1.27
(define/contract (flatten lst)
  (-> list? list?)
  (if (null? lst)
      null
      (let ([c (car lst)]
            [cd (cdr lst)])
        (if (list? c)
            (append (flatten c)
                    (flatten cd))
            (cons c (flatten cd)))))
  )
(module+ test
  (check-equal? (flatten '((1 2) (3 4)))
                '(1 2 3 4))
  (check-equal? (flatten '((x (y)) () z))
                '(x y z))
  )

;; Exercise 1.28
(define/contract (merge loi1 loi2)
  (-> (listof integer?) (listof integer?) (listof integer?))
  (cond
    [(null? loi1) loi2]
    [(null? loi2) loi1]
    [else (let ([c1 (car loi1)]
                [c2 (car loi2)])
            (if (< c1 c2)
                (cons c1 (merge (cdr loi1) loi2))
                (cons c2 (merge loi1 (cdr loi2)))))]
    )
  )
(module+ test
  (check-equal? (merge '(1 4) '(1 2 8))
                '(1 1 2 4 8))
  (check-equal? (merge '(35 62 81 90 91) '(3 83 85 90))
                '(3 35 62 81 83 85 90 90 91))
  )

;; Exercise 1.29
(define/contract (qsort lon)
  (-> (listof number?) (listof number?))
  (sort/predicate < lon)
  )
(module+ test
  (let ([lon '(8 2 5 2 3)])
    (check-equal? (qsort lon)
                  (sort lon <)))
  )

;; Exercise 1.30
(define/contract (sort/predicate pred lon)
  (-> procedure? (listof number?) (listof number?))
  (if (empty? lon)
      null
      (let ([base (car lon)])
        (append (sort/predicate pred (filter (lambda (n) (pred n base)) (cdr lon)))
                (list base)
                (sort/predicate pred (filter (lambda (n) (not (pred n base))) (cdr lon))))))
  )
(module+ test
  (let ([lon '(8 2 5 2 3)])
    (check-equal? (sort/predicate < lon)
                  (sort lon <))
    (check-equal? (sort/predicate > lon)
                  (sort lon >)))
  )

;; Exercise 1.31
;; Bintree ::= Int | (Symbol Bintree Bintree)
(provide (contract-out [struct node
                         ([content symbol?]
                          [left bintree?]
                          [right bintree?])]
                       [struct leaf
                         ([content integer?])]
                       ))
;; contructor
(struct node (content left right) #:transparent)
(struct leaf (content) #:transparent)

;; predicate
(define (bintree? v)
  (-> any/c boolean?)
  (or (leaf? v)
      (node? v))
  )
(module+ test
  (check-true (bintree? (leaf 1)))
  (check-true (bintree? (node 'a (leaf 1) (leaf 6))))
  )

;; extractor
(define/contract (content-of bt)
  (-> bintree? (or/c symbol? integer?))
  (if (leaf? bt)
      (leaf-content bt)
      (node-content bt))
  )
(module+ test
  (check-equal? (content-of (leaf 2)) 2)
  (check-equal? (content-of (node 'a (leaf 2) (leaf 6)))
                'a)
  )

;; Exercise 1.32
(define/contract (double-tree bt)
  (-> bintree? bintree?)
  (if (leaf? bt)
      (leaf (* (content-of bt) 2))
      (node (content-of bt)
            (double-tree (node-left bt))
            (double-tree (node-right bt))))
  )
(module+ test
  (check-equal? (double-tree (leaf 1)) (leaf 2))
  (check-equal? (double-tree (node 'a (leaf 2) (leaf 3)))
                (node 'a (leaf 4) (leaf 6)))
  )
