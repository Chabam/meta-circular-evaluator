#lang racket

(require "../expression.rkt")

(provide assignment?)
(provide assignment-variable)
(provide assignment-value)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))
