#cs
(module yacc mzscheme
  
  (require-for-syntax "private-yacc/parser-builder.ss"
                      "private-yacc/yacc-helper.ss")
  (require "private-yacc/parser-actions.ss"
           "private-yacc/array2d.ss"
           "private-lex/token.ss")
  
  (provide parser)
  
  (define-syntax (parser stx)
    (syntax-case stx ()
      ((_ args ...)
       (let ((arg-list (syntax->list (syntax (args ...))))
             (src-pos #f)
             (debug #f)
             (error #f)
             (tokens #f)
             (start #f)
             (end #f)
             (precs #f)
             (suppress #f)
             (grammar #f))
         (for-each
          (lambda (arg)
            (syntax-case* arg (debug error tokens start end precs grammar suppress src-pos)
              (lambda (a b)
                (eq? (syntax-object->datum a) (syntax-object->datum b)))
              ((debug filename)
               (cond
                 ((not (string? (syntax-object->datum (syntax filename))))
                  (raise-syntax-error 
                   'parser-debug
                   "Debugging filename must be a string"
                   (syntax filename)))
                 (debug
                  (raise-syntax-error #f "Multiple debug declarations" stx))
                 (else
                  (set! debug (syntax-object->datum (syntax filename))))))
              ((suppress)
               (set! suppress #t))
              ((src-pos)
               (set! src-pos #t))
              ((error expression)
               (if error
                   (raise-syntax-error #f "Multiple error declarations" stx)
                   (set! error (syntax expression))))
              ((tokens def ...)
               (if tokens
                   (raise-syntax-error  #f "Multiple tokens declarations" stx)
                   (set! tokens arg)))
              ((start symbol)
               (cond
                 ((not (identifier? (syntax symbol)))
                  (raise-syntax-error
                   'parser-start
                   "Start non-terminal must be a symbol"
                   (syntax symbol)))
                 (start
                  (raise-syntax-error #f "Multiple start declarations" stx))
                 (else
                  (set! start (syntax symbol)))))
              ((end symbols ...)
               (begin
                 (for-each
                  (lambda (sym)
                    (if (not (identifier? sym))
                        (raise-syntax-error
                         'parser-end
                         "End token must be a symbol"
                         sym)))
                  (syntax->list (syntax (symbols ...))))
                 (let ((d (duplicate-list? (syntax-object->datum 
                                            (syntax (symbols ...))))))
                   (if d
                       (raise-syntax-error
                        'parser-end
                        (format "Duplicate end token definition for ~a" d)
                        arg)))
                 (if (= 0 (length (syntax->list (syntax (symbols ...)))))
                     (raise-syntax-error
                      'parser-end
                      "end declaration must contain at least 1 token"
                      arg))
                 (if end
                     (raise-syntax-error #f "Multiple end declarations" stx))
                 (set! end (syntax->list (syntax (symbols ...))))))
              ((precs decls ...)
               (if precs
                   (raise-syntax-error #f "Multiple precs declarations" stx)
                   (set! precs arg)))
              ((grammar prods ...)
               (if grammar
                   (raise-syntax-error #f "Multiple grammar declarations" stx)
                   (set! grammar arg)))
              (_ (raise-syntax-error 'parser-args "argument must match (debug filename), (error expression), (tokens def ...), (start non-term), (end tokens ...), (precs decls ...), or  (grammar prods ...)" arg))))
          (syntax->list (syntax (args ...))))
         (if (not tokens)
             (raise-syntax-error #f "missing tokens declaration" stx))
         (if (not error)
             (raise-syntax-error #f "missing error declaration" stx))
         (if (not grammar)
             (raise-syntax-error #f "missing grammar declaration" stx))
         (if (not end)
             (raise-syntax-error #f "missing end declaration" stx))
         (if (not start)
             (raise-syntax-error #f "missing start declaration" stx))
         (build-parser (if debug debug "")
                       src-pos
                       suppress
                       error
                       tokens
                       start
                       end
                       precs
                       grammar
                       #'here
                       stx)))
      (_
       (raise-syntax-error
        #f
        "parser must have the form (parser args ...)"
        stx))))


)