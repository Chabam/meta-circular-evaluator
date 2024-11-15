#lang racket

(require compatibility/mlist)

(require "expression.rkt")

(provide make-thunk)
(provide thunk?)
(provide thunk-exp)
(provide thunk-env)
(provide make-evaluated-thunk!)
(provide evaluated-thunk?)
(provide thunk-value)

(define (make-thunk exp env)
  (mlist 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

(define (make-evaluated-thunk! thunk result)
  (set-mcar! thunk 'evaluated-thunk)
  (set-mcar! (mcdr thunk)
             result)
  ; replace exp with its value
  (set-mcdr! (mcdr thunk)
             '()))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))

