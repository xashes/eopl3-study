#lang typed/racket

;; Exercise 2.18
;; NodeInSequence ::= (Int Listof(Int) Listof(Int))

(define-type binode (List Integer (Listof Integer) (Listof Integer)))

(: number->sequence (-> Integer binode))
(define (number->sequence n)
  (list n null null))

;; 要先写构造函数，之后所有涉及构造者，皆调用此函数
(: make-binode (-> Integer (Listof Integer) (Listof Integer) binode))
(define (make-binode n loi1 loi2)
  (list n loi1 loi2))

(: current-element (-> binode Integer))
(define (current-element bnd)
  (first bnd))

(: left-side (-> binode (Listof Integer)))
(define (left-side bnd)
  (second bnd))


(: right-side (-> binode (Listof Integer)))
(define (right-side bnd)
  (third bnd))

(: insert-to-left (-> Integer binode binode))
(define (insert-to-left n bnd)
  (list (current-element bnd) (cons n (left-side bnd)) (right-side bnd))
  )

(: insert-to-right (-> Integer binode binode))
(define (insert-to-right n bnd)
  (list (current-element bnd) (left-side bnd) (cons n (right-side bnd)))
  )

(: at-left-end? (-> binode Boolean))
(define (at-left-end? bnd)
  (empty? (left-side bnd)))

(: at-right-end? (-> binode Boolean))
(define (at-right-end? bnd)
  (empty? (right-side bnd)))

(: move-to-left (-> binode binode))
(define (move-to-left bnd)
  (if (at-left-end? bnd)
      (error 'move-to-left "~s is at the left end" bnd)
      (let ([n (current-element bnd)]
            [left (left-side bnd)]
            [right (right-side bnd)])
        (make-binode (car left) (cdr left) (cons n right))))
  )

(: move-to-right (-> binode binode))
(define (move-to-right bnd)
  (let ([n (current-element bnd)]
        [left (left-side bnd)]
        [right (right-side bnd)])
    (make-binode (car right) (cons n left) (cdr right)))
  )

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([n1 (number->sequence 7)]
         [n2 (make-binode 7 '(6 5 4) '(8 9))]
         [n3 (make-binode 7 '() '(8 9))]
         [n4 (make-binode 7 '(6 5 4) '())]
         )
    (check-equal? (current-element n1) 7)
    (check-equal? (make-binode 7 null null) n1)
    (check-equal? (left-side n1)'())
    (check-equal? (right-side n1)'())
    (check-equal? (insert-to-left 9 '(6 (5 4 3) (7 8)))
                  '(6 (9 5 4 3) (7 8)))
    (check-equal? (insert-to-right 9 '(6 (5 4 3) (7 8)))
                  '(6 (5 4 3) (9 7 8)))
    (check-true (at-left-end? '(9 () (1 2 3))))
    (check-false (at-left-end? '(9 (8 7) ())))
    (check-true (at-right-end? '(9 (8 7) ())))

    (check-equal? (move-to-left n2)
                  (make-binode 6 '(5 4) '(7 8 9)))
    (check-equal? (move-to-right n2)
                  (make-binode 8 '(7 6 5 4) '(9)))
    (check-exn exn:fail? (lambda () (move-to-left n3)))
    (check-exn exn:fail? (lambda () (move-to-right n4)))
    ))
