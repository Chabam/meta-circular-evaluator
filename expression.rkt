#lang racket

(provide self-evaluating?)
(provide quoted?)
(provide text-of-quotation)
(provide tagged-list?)
(provide variable?)
(provide true?)
(provide false?)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (cond ((pair? exp) (eq? (car exp) tag))
        ((mpair? exp) (eq? (mcar exp) tag))
        (else false)))

(define (variable? exp) (symbol? exp))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))
