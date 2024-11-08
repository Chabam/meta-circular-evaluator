#lang racket

(require "expression.rkt")

(provide make-procedure)
(provide compound-procedure?)
(provide procedure-parameters)
(provide procedure-body)
(provide procedure-environment)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

