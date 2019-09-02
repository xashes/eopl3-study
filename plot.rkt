#lang racket

(require plot
         racket/math)

(define (prime-dis x)
  (exp x))

(plot3d (surface3d (lambda (x y) (* x y))
                   0 10 0 10)
        )
