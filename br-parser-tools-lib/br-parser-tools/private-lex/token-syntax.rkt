#lang racket/base
(provide make-terminals-def terminals-def-t terminals-def?
         make-e-terminals-def e-terminals-def-t e-terminals-def?)

;; The things needed at compile time to handle definition of tokens
(define-struct terminals-def (t))
(define-struct e-terminals-def (t))
