#lang racket
(require rackunit)

;; in-S? : N -> Bool
;; usage: (in-S? n) = #t if n is in S, #f otherwise
(define in-S?
  (lambda (n)
    (cond
      [(= n 0) #t]
      [(< n 0) #f]
      [else (in-S? (- n 3))]
      )))

;; test
(check-equal? (in-S? 0) #t)
(check-equal? (in-S? -3) #f)
(check-equal? (in-S? 5) #f)
(check-equal? (in-S? 9) #t)

;; list of integers
;; Definition 1.1.3
;; 1. it is the empty list, or
;; 2. it is a pair whose car is an integer and whose cdr is a list of integers
;; list-of-int? : list -> Bool
;; usage: (list-of-int? ls) = #t if ls is a list of integers, #f otherwise
(define list-of-int?
  (lambda (ls)
    (cond
      [(empty? ls) #t]
      [(integer? (car ls)) (list-of-int? (cdr ls))]
      [else #f]
      )))

;; test
(check-equal? (list-of-int? '(1 2 6 7)) #t)
(check-equal? (list-of-int? '()) #t)
(check-equal? (list-of-int? '(1 2 a)) #f)
(check-equal? (list-of-int? '(1 2 1.0)) #t)
(check-equal? (list-of-int? '(1 2 1.6)) #f)

;; exercise 1.1
;; 2. {2n + 3m + 1 | n, m in N}
(define in-T?
  (lambda (n)
    (cond
      [(= n 1) #t]
      [(< n 1) #f]
      [else (or (in-T? (- n 2))
                (in-T? (- n 3)))]
      )))

;; test
(check-equal? (in-T? 2) #f)
(check-equal? (in-T? 3) #t)
(check-equal? (in-T? 4) #t)
(check-equal? (in-T? 5) #t)
(check-equal? (in-T? 6) #t)
(check-equal? (in-T? 7) #t)
(check-equal? (in-T? 8) #t)

;; 3. {(n, 2n + 1) | n in N}
(define in-U?
  (lambda (n m)
    (cond
      [(and (= n 0) (= m 1)) #t]
      [(or (< n 0) (< m 1)) #f]
      [else (in-U? (- n 1) (- m 2))])))

(check-equal? (in-U? 0 1) #t)
(check-equal? (in-U? 0 3) #f)
(check-equal? (in-U? 1 3) #t)
(check-equal? (in-U? 0 5) #f)

;; 4. {(n, n^2) | n in N}
(define in-V?
  (lambda (n m)
    (cond
      [(and (= n 0) (= m 0))]
      [(or (< n 0) (< m 0)) #f]
      [else (in-V? (- n 1)
                   (+ (- m
                         (* 2 n))
                      1))]
      )))

(check-equal? (in-V? 0 0) #t)
(check-equal? (in-V? 3 9) #t)
(check-equal? (in-V? 3 16) #f)

;; Exercise 1.2 sets defined by rules
;; 1. {n, 7n + 1 | n in N}
(define in-W?
  (lambda (n m)
    (cond
      [(and (= n 0)
            (= m 1))]
      [(or (< n 0) (< m 1)) #f]
      [else (in-W? (- n 1) (- m 7))]
      )))

(check-equal? (in-W? 1 8) #t)
(check-equal? (in-W? 2 15) #t)

;; lambda calculus
((lambda (x) ((lambda (y) (+ x y)) 1)) 2)

;; deriving recursive programs
;; List ::= () | (Scheme value . List)
;; list-length: List -> Int
;; usage: (list-length l) = the length of l
(define list-length
  (lambda (lst)
    (cond
      [(empty? lst) 0]
      [else (+ 1 (list-length (rest lst)))]
      )))

(check-equal? (list-length '()) 0)
(check-equal? (list-length '(a b c)) 3)
(check-equal? (list-length '('(a b c) b c)) 3)

;; nth-element: List Int -> Scheme value
;; usage: (nth-element l n) -> the nth element of l
(define (nth-element lst n)
  (let nth-element-impl ([lst-in lst] [n-in n])
    (if (empty? lst-in)
        (report-list-too-short lst n)
        (if (= n-in 0)
            (first lst-in)
            (nth-element-impl (rest lst-in) (- n-in 1))))))

(define (report-list-too-short lst n)
  (error 'nth-element
         "~a does not have ~a elements\n" lst (+ n 1))
  )

(check-equal? (nth-element '(a b c) 1) 'b)
(check-equal? (nth-element '(a b c) 0) 'a)

;; remove-first: Symbol List-of-Symbols -> List-of-Symbols
;; usage: (remove-first s los) -> a list with 1st occurence of s removed from los
(define (remove-first s los)
  (cond
    [(empty? los) '()]
    [(equal? s (car los)) (cdr los)]
    [else (cons (car los) (remove-first s (cdr los)))]))

(check-equal? (remove-first 'a '(a b c)) '(b c))
(check-equal? (remove-first 'b '(e f g)) '(e f g))
(check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
(check-equal? (remove-first 'x '()) '())

;; remove-all: Sym Listof(Sym) -> Listof(Sym)
;; usage: (remove-all s los) -> a list with all occurence of s removed from los
;; Listof(Sym) :== () | (Sym . Listof(Sym))
(define (remove-all s los)
  (if (empty? los)
      '()
      (if (equal? (car los) s)
          (remove-all s (cdr los))
          (cons (car los) (remove-all s (cdr los))))))

(check-equal? (remove-all 'a '(c a b a)) '(c b))

;; occurs-free? : Sym LcExp -> Boll
;; usage: (occurs-free? s exp) -> #t if s occurs free in exp, #f otherwise
;; LcExp ::= Identifier
;;         | (lambda (Identifier) LcExp)
;;         | (LcExp LcExp)
;; To be improved in section 2.5 with readability
(define (occurs-free? s exp)
  (cond
    [(symbol? exp) (eq? s exp)]
    [(eq? (car exp) 'lambda) (and (not (eq? s (caadr exp))) (occurs-free? s (caddr exp)))]
    [else (or (occurs-free? s (car exp)) (occurs-free? s (cadr exp)))]
    ))

(check-equal? (occurs-free? 'x 'x) #t)
(check-equal? (occurs-free? 'x 'y) #f)
(check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f)
(check-equal? (occurs-free? 'x '(lambda (y) (x y))) #t)
(check-equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t)
(check-equal? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)

;; subst : Sym Sym S-List -> S-List
;; usage: (subst new old slist) -> a new list that is similar to slist
;; but with all ocurrence of old replace by instances of new
;; ------------------------------------------------------------
;; S-list ::= ({S-exp}*)
;; S-exp ::= Sym | S-list
;; ------------------------------------------------------------
;; S-list ::= ()
;;          | (S-exp . S-list)
;; S-exp ::= Sym | S-list
;; (define (subst new old slist)
;;   (if (empty? slist)
;;       '()
;;       (cons (subst-sexp new old (car slist))
;;             (subst new old (cdr slist)))))

(define (subst-sexp new old sexp)
  (if (symbol? sexp)
      (if (eq? old sexp)
          new
          sexp)
      (subst new old sexp)))

(define (subst2 new old slist)
  (cond
    [(empty? slist) '()]
    [(symbol? (car slist)) (cons (if (eq? old (car slist))
                                     new
                                     (car slist))
                                 (subst2 new old (cdr slist)))]
    [else (cons (subst2 new old (car slist))
                (subst2 new old (cdr slist)))]
    ))

(define (subst new old slist)
  (map (lambda (sexp)
         (subst-sexp new old sexp))
       slist)
  )

;; number-elements : Listof(SchemeVal) -> Listof(List(Int SchemeVal))
;; usage: (number-elements lst) -> '((0 v0) (1 v1) ...)
;; Auxiliary Procedure
;; number-elements-from : Listof(SchemeVal) Int -> Listof(List(Int SchemeVal))
;; usage: (number-elements-from '(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
;; Listof(SchemeVal) ::= () | (SchemeVal . Listof(SchemeVal))
;; lst - the list we are working on. It get smaller at every recursive call.
;; n - an abstract context in which we are working - be called context argument or inherited attribute
(define (number-elements-from lst n)
  (if (empty? lst)
      '()
      (cons (list n (car lst))
            (number-elements-from (cdr lst) (+ n 1))))
  )

(define (number-elements lst)
  (let number-elements-from ([lst lst] [n 0])
    (if (empty? lst)
        '()
        (cons (list n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))

(define (number-elements-map lst)
  (map (lambda (i v) (list i v))
       (range (length lst))
       lst)
  )

(check-equal? (number-elements '(a b c)) '((0 a) (1 b) (2 c)))
(check-equal? (number-elements-map '(a b c)) '((0 a) (1 b) (2 c)))

;; Exercise 1.36 Write a procedure g such that number-elements from page 23
;; could be defined as
;; (define number-elements
;;   (lambda (lst)
;;     (if (null? lst) ’()
;;         (g (list 0 (car lst)) (number-elements (cdr lst))))))


;; list-sum : Listof(Numbers) -> Number
;; usage: (list-sum lst) -> sum of the values in lst
;; Listof(Numbers) ::= () | (Number . Listof(Numbers))
(define (list-sum lst)
  (if (empty? lst)
      0
      (+ (car lst) (list-sum (cdr lst)))))

(check-equal? (list-sum '(1 2 3)) 6)

;; Exercise 1.15
;; duple : Int SchemeVal -> Listof(SchemeVal)
;; usage: (duple n x) -> a list containing n copies of x
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (sub1 n) x))))

(check-equal? (duple 2 3) '(3 3))
(check-equal? (duple 2 '(ha ha))
              '((ha ha) (ha ha)))
(check-equal? (duple 0 '(blah)) '())

;; Exercise 1.16
;; invert : Listof(Listof(Val Val)) -> Listof(Listof(Val Val))
;; usage: (invert lst) -> a list with each 2-list reversed
;; Listof(Listof(Val Val)) ::= () | (Listof(Val Val) . Listof(Listof(Val Val)))
(define (invert lst)
  (if (empty? lst)
      '()
      (cons (list (cadar lst) (caar lst))
            (invert (cdr lst)))))

(define (invert-map lst)
  (map (lambda (pair) (list (cadr pair) (car pair)))
       lst))

(check-equal? (invert-map '((a 1) (a 2) (1 b) (2 b)))
              '((1 a) (2 a) (b 1) (b 2)))

;; Exercise 1.17
;; down : List -> List
;; usage: (down lst) -> wraps parentheses around each top-level element of lst
;; List ::= () | (Val . List)
;; (define (down lst)
;;   (if (empty? lst)
;;       '()
;;       (cons (list (car lst))
;;             (down (cdr lst)))))

(define (down lst)
  (map list lst))

(check-equal? (down '(1 2 3)) '((1) (2) (3)))

;; Exercise 1.18
;; swapper : Sym Sym s-list -> s-list
;; usage: (swapper s1 s2 slist) -> a list the same as slist,
;; but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1
;; s-list :== () | (s-exp . s-list)
;; s-exp :== Symbol | s-list

;; (define (swapper s1 s2 slist)
;;   (if (empty? slist)
;;       '()
;;       (cons (swapper-sexp s1 s2 (car slist))
;;             (swapper s1 s2 (cdr slist)))))

(define (swapper-sexp s1 s2 sexp)
  (if (symbol? sexp)
      (cond
        [(eq? sexp s1) s2]
        [(eq? sexp s2) s1]
        [else sexp]
        )
      (swapper s1 s2 sexp)))

;; s-list ::= ({s-exp}*)
(define (swapper s1 s2 slist)
  (map (lambda (sexp) (swapper-sexp s1 s2 sexp))
       slist))

(check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))

;; Exercise 1.19
;; list-set : Listof(Val) Int Val -> Listof(Val)
;; usage: (list-set-ex lst n x) -> a list like lst, except that the n-th element is x
;; Listof(Val) ::= () | (Val . Listof(Val))
(define (list-set-ex lst n x)
  (if (empty? lst)
      '()
      (if (zero? n)
          (cons x (cdr lst))
          (cons (car lst) (list-set-ex (cdr lst) (sub1 n) x)))))

(check-equal? (list-set-ex '(a b c d) 2 '(1 2)) '(a b (1 2) d))

;; Exercise 1.20
;; count-occurrences : Sym S-list -> Int
;; usage: (count-occurrences s slist) -> the number of occurrences of s in slist
;; S-list ::= ({S-exp*})
;; S-exp ::= Symbol | S-list
(define (count-occurrences-in-sexp s sexp)
  (if (symbol? sexp)
      (if (eq? s sexp)
          1
          0)
      (count-occurrences s sexp)))

(define (count-occurrences s slist)
  (if (empty? slist)
      0
      (+ (count-occurrences-in-sexp s (car slist))
         (count-occurrences s (cdr slist)))))

(check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)

;; Exercise 1.21
;; product : List(Sym) List(Sym) -> List(List(Sym Sym))
;; usage: (product sos1 sos2) -> a list of 2-lists that represents the Cartesian product
;; of sos1 and sos2.
(define (product-for* sos1 sos2)
  (for*/list ([i sos1]
              [j sos2])
    (list i j))
  )
(define (product-for sos1 sos2)
  (for/list ([i sos1])
    (for/list ([j sos2])
      (list i j))))

(define (product-sym s sos)
  (if (empty? sos)
      '()
      (cons (list s (car sos))
            (product-sym s (cdr sos)))))

(define (product sos1 sos2)
  (if (empty? sos1)
      '()
      (append (product-sym (car sos1) sos2)
              (product (cdr sos1) sos2))))


(check-equal? (product '(a b c) '(x y))
              '((a x) (a y) (b x) (b y) (c x) (c y)))

;; Exercise 1.22
;; filter-in : procedure List(Val) -> List(Val)
;; usage: (filter-in pred lst) -> the list of those elements in lst that satisfy the predicate pred
;; List(Val) ::= () | (Val . List(Val))
(define (filter-in pred lst)
  (if (empty? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))

(define (filter-for pred lst)
  (for/list ([i lst]
             #:when (pred i))
    i
    ))

(check-equal? (filter-in number? '(a 2 (1 3) b 7))
              '(2 7))

;; Exercise 1.23
;; list-index : proc List -> Int or #f
;; (list-index pred lst) -> the position of the first element of lst that satisfies the predicate pred.
;;                          If no element of lst satisfies the predicate pred, then returns #f.
;; List(Val) ::= () | (Val . List(Val))
(define (list-index pred lst)
  (list-index-from pred lst 0))

(define (list-index-from pred lst n)
  (if (empty? lst)
      #f
      (if (pred (car lst))
          n
          (list-index-from pred (cdr lst) (+ n 1)))))

(check-equal? (list-index number? '(a (1 3) 2 b 7))
2)
(check-equal? (list-index symbol? '(1 2 (a b) 3))
              #f)

;; Exercise 1.24
;; every? : predicate List -> Bool
;; usage: (every? pred lst) -> #f if any element of lst fails to satisfy pred, #t otherwise
(define (every? pred lst)
  (if (empty? lst)
      #t
      (and (pred (car lst))
           (every? pred (cdr lst)))))

(define (every?-map pred lst)
  (andmap pred lst))

(check-equal? (every?-map number? '(a b c 3 e))
              #f)
(check-equal? (every?-map number? '(1 2 3 5 4))
              #t)

;; Exercise 1.25
;; exists? : predicate List -> Bool
;; usage: (exists? pred lst) -> #t if any element of lst satisfies pred, #f otherwise
(define (exists? pred lst)
  (if (empty? lst)
      #f
      (or (pred (car lst))
          (exists? pred (cdr lst)))))

(check-equal? (exists? number? '(a b c 3 e))
              #t)
(check-equal? (exists? number? '(a b c d e))
              #f)
;; Exercise 1.26
;; up : List -> List
;; usage: (up lst) -> removes a pair of parentheses from each top-level element of lst.
;;                    If a top-level element is not a list, it is included in the result,
;;                    as it is.
(define (up lst)
  (if (empty? lst)
      '()
      (let ([head (car lst)]
            [tail (cdr lst)])
        (if (list? head)
            (append head
                    (up tail))
            (cons head
                  (up tail))))))

(check-equal? (up '((1 2) (3 4)))
              '(1 2 3 4))
(check-equal? (up '((x (y)) z))
              '(x (y) z))

;; Exercise 1.27
;; myflatten : slist -> list?
;; usage : (myflatten slist) -> a list with all the inner parentheses removed from its argument
;; S-list ::= ({S-exp}*)
;; S-list ::= () | (S-exp . S-list)
;; S-exp ::= Symbol | S-list
(define (myflatten slist)
  (if (empty? slist)
      '()
      (if (symbol? (car slist))
          (cons (car slist) (myflatten (cdr slist)))
          (myflatten (append (car slist) (cdr slist)))
          )
      ))

(let ([lst1 '(a (b) ((c)) (d e))]
      [lst2 '(() (a) () (()) (b ()))])
  (check-equal? (myflatten lst1) '(a b c d e))
  (check-equal? (myflatten lst2) '(a b)))

;; Exercise 1.28
;; merge : list(Int) list(Int) -> list(Int)
;; usage: (merge loi1 loi2) -> loi1 and loi2 are lists of integers that sorted in ascending order,
;; returns a sorted list of all integers in loi1 and loi2.
;; List(Int) ::= () | (Int . List(Int))
(define (insert-int loi s)
  (cond
    [(empty? loi) (list s)]
    [(< s (car loi)) (cons s loi)]
    [else (cons (car loi) (insert-int (cdr loi) s))]))

(define (merge loi1 loi2)
  (if (empty? loi2)
      loi1
      (merge (insert-int loi1 (car loi2)) (cdr loi2))))

(check-equal? (insert-int '(8 9 16) 9) '(8 9 9 16))
(check-equal? (merge '(35 62 81 90 91) '(3 83 85 90))
              '(3 35 62 81 83 85 90 90 91))

;; Exercise 1.29
;; eopl-sort : List(Int) -> List(Int)
;; usage: (eopl-sort loi) -> a list of the elements of loi in ascending order
;; List(Int) ::= () | (Int . List(Int))
(define (eopl-sort loi)
  (let sort-n ([n 1]
               [lst loi])
    (if (= n (length lst))
        lst
        (sort-n (+ n 1) (swap < lst)))))

(define (swap pred loi)
  (if (empty? loi)
      '()
      (let ([h (car loi)]
            [t (cdr loi)])
        (if (empty? t)
            (list h)
            (if (pred h (car t))
                (cons h (swap pred t))
                (cons (car t) (swap pred (cons h (cdr t))))
                )))
      ))

(check-equal? (eopl-sort '(8 2 5 2 3))
              '(2 2 3 5 8))
(check-equal? (eopl-sort '(8 7 6 5))
              '(5 6 7 8))

;; Exercise 1.30
(define (sort/predicate pred loi)
  (let sort-n ([n 1]
               [lst loi])
    (if (= n (length lst))
        lst
        (sort-n (add1 n) (swap pred lst)))))

(check-equal? (sort/predicate < '(8 2 5 2 3))
              '(2 2 3 5 8))
(check-equal? (sort/predicate > '(8 2 5 2 3))
              '(8 5 3 2 2))

;; Exercise 1.31 Write the following procedures for calculating on a bintree (definition 1.1.7):
;; leaf and interior-node, which build bintrees, leaf?, which tests
;; whether a bintree is a leaf, and lson, rson, and contents-of, which extract the
;; components of a node. contents-of should work on both leaves and interior
;; nodes.

;; Bintree ::= Int | (Symbol Bintree Bintree)
(define (leaf n)
  n)

;; interior-node : Symbol Bintree Bintree -> 3 elements list
;; usage: (interior-node s bt1 bt2) -> a 3 elements list
(define (interior-node s bt1 bt2)
  (list s bt1 bt2))

(define leaf? integer?)

(define lson second)

(define rson third)

(define (contents-of node)
  (if (leaf? node)
      node
      (car node)))

;; Exercise 1.32
;; double-tree : Bintree -> Bintree
;; usage: (double-tree bt) -> a bintree like the original, but with all the integers in the
;; leaves doubled
(define (double-tree bt)
  (if (leaf? bt)
      (leaf (* 2 (contents-of bt)))
      (interior-node (contents-of bt) (double-tree (lson bt)) (double-tree (rson bt)))))

(check-equal? (double-tree 1) 2)
(check-equal? (double-tree '(foo (bar 1 3) 6))
              '(foo (bar 2 6) 12))

;; Exercise 1.33
;; mark-leaves-with-red-depth : Bintree -> Bintree
;; usage: (mark-leaves-with-red-depth bt) -> a new tree, each leaf contains the number of nodes
;; between it and the root that contain the symbol red.
;; Bintree ::= Int | (Symbol Bintree Bintree)
(define (mark-leaves-with-red-depth bintree)
  (let mark-helper ([bt bintree]
                    [n 0])
    (if (leaf? bt)
        (leaf n)
        (if (eq? (contents-of bt) 'red)
            (interior-node 'red
                           (mark-helper (lson bt) (add1 n))
                           (mark-helper (rson bt) (add1 n)))
            (interior-node (contents-of bt)
                           (mark-helper (lson bt) n)
                           (mark-helper (rson bt) n)
                           )
            ))))

(check-equal? (mark-leaves-with-red-depth
               (interior-node 'red
                              (interior-node 'bar
                                             (leaf 26)
                                             (leaf 12))
                              (interior-node 'red
                                             (leaf 11)
                                             (interior-node 'quux
                                                            (leaf 117)
                                                            (leaf 14)))))
              '(red
                (bar 1 1)
                (red 2 (quux 2 2))))

;; Exercise 1.34
;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)
;; Write a procedure path that takes an integer n and a binary
;; search tree bst (page 10) that contains the integer n, and returns a list of lefts and
;; rights showing how to find the node containing n. If n is found at the root, it returns
;; the empty list.
;; path : Int BST -> List
;; usage: (path n bst) -> a list of lefts and rights showing how to find the node containing n.
;; note: Know the data
(define (path n bst)
  (if (empty? bst)
      (error 'path "The integer ~a is not found" n)
      (let ([root (contents-of bst)]
            [rt (rson bst)]
            [lt (lson bst)])
        (cond
          [(= n root) '()]
          [(< n root) (cons 'left (path n lt))]
          [else (cons 'right (path n rt))]
          )))
  )

(check-equal? (path 17 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(right left left))

;; Exercise 1.35
;; number-leaves : Bintree -> Bintree
;; usage: (number-leaves bt) -> the contents of the leaves are numbered starting from 0
;; Bintree ::= Int | (Symbol Bintree Bintree)
(define (number-leaves btree)
  (let number-from ([bt btree]
                    [n 0])
    (if (leaf? bt)
        (leaf n)
        (interior-node (contents-of bt)
                       (number-from (lson bt) n)
                       (number-from (rson bt) (add1 n)))
        )
    ))

(define (leaves btree)
  (if (leaf? btree)
      btree
      (cons (leaves (lson btree)) (cons (leaves (rson btree)) '())))
  )

(check-equal?
 (leaves
  (interior-node 'foo
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'baz
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))
 '(foo
   (bar 0 1)
   (baz
    2
    (quux 3 4))))
