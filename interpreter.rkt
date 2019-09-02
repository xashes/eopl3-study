#lang racket

(require "env-alist.rkt")

(empty-env)
(extend-env 'a 1 (empty-env))

