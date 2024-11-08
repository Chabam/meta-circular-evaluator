#lang racket

(require "../expression.rkt")

(provide make-begin)
(provide begin?)
(provide begin-actions)

(define (make-begin seq) (cons 'begin seq))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
