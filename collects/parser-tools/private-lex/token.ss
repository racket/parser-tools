#cs
(module token mzscheme

  ;; Defining tokens
  
  (require-for-syntax "token-syntax.ss")

  (provide define-tokens define-empty-tokens make-token token-name token-value token?)

  (define-struct token (name value) (make-inspector))

  (define-syntax (define-tokens stx)
    (syntax-case stx ()
      ((_ name ...)
       (define-tokens-helper stx #'here #f))))
  
  (define-syntax (define-empty-tokens stx)
    (syntax-case stx ()
      ((_ name ...)
       (define-tokens-helper stx #'here #t))))
)

