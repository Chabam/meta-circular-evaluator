#lang racket

(require "expression-types/begin.rkt")

(provide last-exp?)
(provide first-exp)
(provide rest-exps)
(provide sequence->exp)

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
