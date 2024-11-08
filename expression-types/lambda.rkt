#lang racket

(require "../expression.rkt")

(provide make-lambda)
(provide lambda?)
(provide lambda-parameters)
(provide lambda-body)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

