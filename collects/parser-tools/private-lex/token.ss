#cs
(module token mzscheme

  ;; Defining tokens
  
  (require-for-syntax "token-syntax.ss")

  (provide define-tokens define-empty-tokens token-name token-value token?)

  (define-struct token (name value))

  (define-syntax define-tokens
    (lambda (stx)
      (syntax-case stx ()
	((_ name ...)
	 (define-tokens-helper stx #'hack #f)))))
  
  (define-syntax define-empty-tokens
    (lambda (stx)
      (syntax-case stx ()
	((_ name ...)
	 (define-tokens-helper stx #'hack #t)))))
)

