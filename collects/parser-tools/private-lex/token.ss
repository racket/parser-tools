#cs
(module token mzscheme

  (require-for-syntax "token-syntax.ss")
  
  ;; Defining tokens
  
  (provide define-tokens define-empty-tokens make-token token-name token-value token?)

  (define-struct token (name value) (make-inspector))

  (define-syntaxes (define-tokens define-empty-tokens)
    (let ((define-tokens-helper 
           (lambda (stx empty?)
             (syntax-case stx ()
               ((_ name (terms ...))
                (andmap identifier? (syntax->list (syntax (terms ...))))
                (datum->syntax-object
                 #'here
                 `(begin
                    (define-syntax ,(syntax name)
                      ,(if empty?
                           `(make-e-terminals-def (quote-syntax ,(syntax (terms ...))))
                           `(make-terminals-def (quote-syntax ,(syntax (terms ...))))))
                    ,@(map
                       (lambda (n)
                         (if (eq? (syntax-object->datum n) 'error)
                             (raise-syntax-error
                              #f
                              "Cannot define a token named error."
                              stx))
                         `(define (,(datum->syntax-object 
                                     n
                                     (string->symbol 
                                      (format "token-~a" (syntax-object->datum n))) 
                                     n)
                                   ,@(if empty? '() '(x)))
                            (make-token ',n ,(if empty? #f 'x))))
                       (syntax->list (syntax (terms ...)))))	
                 stx))
               ((_ ...)
                (raise-syntax-error 
                 #f
                 "must have the form (define-tokens name (symbol ...)) or (define-empty-tokens name (symbol ...))"
                 stx))))))
      (values
       (lambda (stx) (define-tokens-helper stx #f))
       (lambda (stx) (define-tokens-helper stx #t)))))
)

