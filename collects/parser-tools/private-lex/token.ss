(module token mzscheme

  (require-for-syntax "token-syntax.ss")
  
  ;; Defining tokens
  
  (provide define-tokens define-empty-tokens make-token token?
           (protect (rename token-name real-token-name))
           (protect (rename token-value real-token-value))
           (rename token-name* token-name)
           (rename token-value* token-value)
           (struct position (offset line col))
           (struct position-token (token start-pos end-pos)))

  
  ;; A token is either
  ;; - symbol
  ;; - (make-token symbol any)
  (define-struct token (name value))

  ;; token-name*: token -> symbol
  (define (token-name* t)
    (cond
      ((symbol? t) t)
      ((token? t) (token-name t))
      (else (raise-type-error 
             'token-name 
             "symbol or struct:token"
             0
             t))))
  
  ;; token-value*: token -> any
  (define (token-value* t)
    (cond
      ((symbol? t) #f)
      ((token? t) (token-value t))
      (else (raise-type-error 
             'token-value
             "symbol or struct:token"
             0
             t))))
  
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
                                     n
                                     n)
                                   ,@(if empty? '() '(x)))
                            ,(if empty?
                                 `',n
                                 `(make-token ',n x))))
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

  (define-struct position (offset line col))
  (define-struct position-token (token start-pos end-pos))
  )

