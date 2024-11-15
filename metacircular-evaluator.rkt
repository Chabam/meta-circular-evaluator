#lang racket

(require compatibility/mlist)

(require "expression.rkt")
(require "environment.rkt")
(require "procedure.rkt")
(require "global-environment.rkt")
(require "sequence.rkt")
(require "thunk.rkt")
(require "expression-types/assignment.rkt")
(require "expression-types/definition.rkt")
(require "expression-types/if.rkt")
(require "expression-types/lambda.rkt")
(require "expression-types/begin.rkt")
(require "expression-types/cond.rkt")

(provide eval)
(provide actual-value)

(define apply-in-underlying-racket apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply-internal (actual-value (operator exp) env)
                         (operands exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply-internal procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (mcons (delay-it (first-operand exps)
                       env)
             (list-of-delayed-args (rest-operands exps)
                                   env))))

(define delay-it make-thunk)

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (make-evaluated-thunk! obj result)
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-racket
   (primitive-implementation proc) args))

'LAZY-METACIRCULAR-EVALUATOR-LOADED
