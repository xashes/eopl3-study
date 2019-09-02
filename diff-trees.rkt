#lang racket

;; Exercise 2.3
;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;; Implementation ::= (zero is-zero? successor predecessor)

;; simplest-representation : Diff-tree -> Diff-tree
;; usage: (simplest-representation dt) -> the simplest representation of a Diff-tree
;; based on (one) is identical to (diff (one) (diff (one) (one)))
;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;; FIXME can't simplify forms like (diff (diff ...) diff-tree)
(define (simplest-representation dt)
  (cond
    [(equal? dt '(one)) '(one)]
    [(equal? dt '(diff (one) (diff (one) (one)))) '(one)]
    [else
     `(diff
       ,(simplest-representation (second dt))
       ,(simplest-representation (third dt)))]
    )
  )

;; zero : () -> Diff-tree
;; usage: (zero) -> a Diff-tree represents zero
(define zero
  (lambda ()
    '(diff (one) (one))))

;; predecessor : Diff-tree -> Diff-tree
;; usage: (predecessor n) -> a representation that represents one less than n
(define predecessor
  (lambda (n)
    `(diff ,n (one))))

;; successor : Diff-tree -> Diff-tree
;; usage: (successor n) -> n plus 1
(define successor
  (lambda (n)
    `(diff ,n (diff ,(zero) (one)))))

;; integer->diff-tree : Int -> Diff-tree
;; usage: (integer->diff-tree n) -> a diff-tree whose value equals to n
(define (integer->diff-tree n)
  (let it-from ([n n]
                [dt '(one)])
    (cond
      [(= n 1) dt]
      [(> n 1) (it-from (sub1 n) (successor dt))]
      [else (it-from (add1 n) (predecessor dt))]
      )))

;; diff-tree->integer : Diff-tree -> Int
;; usage: (diff-tree->integer dt) -> a integer equals to dt's value
(define (diff-tree->integer dt)
  (if (equal? dt '(one))
      1
      (- (diff-tree->integer (second dt))
         (diff-tree->integer (third dt)))))

;; is-zero? : Diff-tree -> Bool
;; usage: (is-zero? dt) -> #t if dt equals to 0, #f otherwise
(define is-zero?
  (lambda (dt)
    (zero? (diff-tree->integer dt))))

;; diff-tree-plus : Diff-tree Diff-tree -> Diff-tree
;; usage: (diff-tree-plus dt1 dt2) -> a diff-tree represents the sum of dt1 and dt2
(define (diff-tree-plus dt1 dt2)
  `(diff ,dt1 (diff ,(zero) ,dt2)))

;; diff-tree=? : Diff-tree Diff-tree -> Bool
;; usage: (diff-tree=? dt1 dt2) -> #t if their values are equal, #f otherwise
(define (diff-tree=? dt1 dt2)
  (= (diff-tree->integer dt1)
     (diff-tree->integer dt2))
  )
;; diff-tree?


(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (simplest-representation '(diff (one) (diff (one) (one))))
                '(one))
  (check-equal? (simplest-representation '(diff (one) (diff (one) (diff (one) (one)))))
                '(diff (one) (one)))

  (check-equal? (predecessor '(one)) '(diff (one) (one)))
  (check-equal? (successor '(one))
                (simplest-representation '(diff (one) (diff (diff (one) (one)) (one)))))

  (check-equal? (simplest-representation (integer->diff-tree -1)) `(diff ,(zero) (one)))
  (check-equal? (diff-tree->integer (integer->diff-tree 10))
                10)
  (check-equal? (diff-tree->integer (integer->diff-tree 0))
                0)

  (check-equal? (diff-tree-plus '(one) '(one))
                (simplest-representation '(diff (one) (diff (diff (one) (one)) (one)))))

  )
