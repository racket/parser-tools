#cs
(module token-syntax mzscheme

  ;; The things needed at compile time to handle definition of tokens
  
  (provide make-terminals-def terminals-def-t terminals-def? define-tokens-helper)

  (define-struct terminals-def (t))

  (define (define-tokens-helper stx runtime empty?)
    (syntax-case stx ()
      ((_ name (terms ...))
       (andmap identifier? (syntax->list (syntax (terms ...))))
       (datum->syntax-object
	runtime
	`(begin
	   (define-syntax ,(syntax name)
	     (make-terminals-def (quote-syntax ,(syntax (terms ...)))))
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
	"must have the form (define-tokens name (symbol ...))"
	stx))))
)
