#cs
(module yacc mzscheme

  (require-for-syntax "private-yacc/parser-builder.ss")
  (require "private-yacc/terminal.ss" 
           "private-yacc/parser-actions.ss"
           "private-yacc/array2d.ss")
  
  (provide define-tokens define-empty-tokens parser parser-debug)

  (define-syntax parser
    (lambda (stx)
      (syntax-case stx ()
	((_ start input-terms assocs prods) 
         (build-parser (syntax start) (syntax input-terms) 
                       (syntax assocs) (syntax prods)
                       "" #'here stx))
        (_
         (raise-syntax-error
          #f
          "parser must have the form (parser start-symbol tokens precedence/associativity productions)"
          stx)))))
        

  (define-syntax parser-debug
    (lambda (stx)
      (syntax-case stx ()
	((_ filename start input-terms assocs prods)
	 (string? (syntax-object->datum (syntax filename)))
         (build-parser (syntax start) (syntax input-terms) 
                       (syntax assocs) (syntax prods)
                       (syntax-object->datum (syntax filename))
                       #'here stx))
        (_
         (raise-syntax-error
          #f
          "parser must have the form (parser-debug filename start-symbol tokens precedence/associativity productions) where filename is a string"
          stx)))))
         
)