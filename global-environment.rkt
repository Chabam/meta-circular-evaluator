#lang racket

(require compatibility/mlist)

(require "environment.rkt")
(require "expression.rkt")

(provide the-global-environment)
(provide primitive-procedure?)
(provide primitive-implementation)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        ;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (cadr proc)))
        (apply mlist primitive-procedures)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcar (mcdr proc)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
