#lang racket

(require "global-environment.rkt")
(require "procedure.rkt")
(require "metacircular-evaluator.rkt")

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define error-prompt ";;; M-Eval error:")


(define (repl)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (with-handlers ((exn? announce-error))
      (let ((output (actual-value input the-global-environment)))
        (announce-output output-prompt)
        (user-print output))))
  (repl))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (announce-error error)
  (newline)
  (display error-prompt)
  (newline)
  (display (exn-message error))
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
(let ((list-lib (read (open-input-file "libraries/list.mceval"))))
  (displayln "Loading list library")
  (actual-value list-lib the-global-environment))
(repl)
