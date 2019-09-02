#lang typed/racket

;; Exercise 2.4, 2.12, 2.22
;; stack ::= () | (SchemeVal . Stack)
;; stack ::= () -> SchemeVal

(define-type Stack (U empty-stack ext-stack))
(struct empty-stack () #:transparent)
(struct ext-stack ([top : Any] [rest : Stack]) #:transparent)

;; constructors
(: push (-> Any Stack Stack))
(define (push val stk)
  (ext-stack val stk))

;; observers
(: top (-> Stack Any))
(define (top stk)
  (if (empty-stack? stk)
      (error 'top "The stack is empty.")
      (ext-stack-top stk))
  )

(: pop (-> Stack Stack))
(define (pop stk)
  (if (empty-stack? stk)
      (error 'pop "The stack is empty.")
      (ext-stack-rest stk)))


(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([s1 (empty-stack)]
         [s2 (push 0 s1)]
         [s3 (push 1 s2)]
         [s4 (pop s3)]
         )
    (check-true (empty-stack? s1))
    (check-equal? (top s2) 0)
    (check-equal? (top s3) 1)
    (check-equal? (top (pop s3)) 0)
    )

  )
