(module parser-builder mzscheme
  
  (require "input-file-parser.ss"
           "grammar.ss"
           "table.ss"
           (lib "class.ss")
           (lib "contract.ss"))
  (require-for-template mzscheme)
  
  (provide/contract
   (build-parser ((string? any? any? (listof identifier?) (listof identifier?)
                  (listof identifier?) (union syntax? false?) syntax?) . ->* .
                 (any? any? any? any?))))
  
  ;; fix-check-syntax : (listof identifier?) (listof identifier?) (listof identifier?)
  ;;                    (union syntax? false?) syntax?) -> syntax?
  (define (fix-check-syntax input-terms start ends assocs prods)
    (let* ((term-binders (get-term-list input-terms))
           (get-term-binder
            (let ((t (make-hash-table)))
              (for-each 
               (lambda (term)
                 (hash-table-put! t (syntax-e term) term))
               term-binders)
              (lambda (x)
                (let ((r (hash-table-get t (syntax-e x) (lambda () #f))))
                  (if r
                      (syntax-local-introduce (datum->syntax-object r (syntax-e x) x x))
                      x)))))
           (rhs-list
            (syntax-case prods ()
              (((_ rhs ...) ...)
               (syntax->list (syntax (rhs ... ...)))))))
      (with-syntax (((tmp ...) (map syntax-local-introduce term-binders))
                    ((term-group ...)
                     (map (lambda (tg)
                            (syntax-property
                             (datum->syntax-object tg #f)
                             'disappeared-use
                             tg))
                          input-terms))
                    ((end ...)
                     (map get-term-binder ends))
                    ((start ...)
                     (map get-term-binder start))
                    ((bind ...)
                     (syntax-case prods ()
                       (((bind _ ...) ...)
                        (syntax->list (syntax (bind ...))))))
                    (((bound ...) ...)
                     (map
                      (lambda (rhs)
                        (syntax-case rhs ()
                          (((bound ...) (_ pbound) __)
                           (map get-term-binder
                                (cons (syntax pbound)
                                      (syntax->list (syntax (bound ...))))))
                          (((bound ...) _)
                           (map get-term-binder
                                (syntax->list (syntax (bound ...)))))))
                      rhs-list))
                    ((prec ...)
                     (if assocs
                         (map get-term-binder
                              (syntax-case assocs ()
                                (((__ term ...) ...)
                                 (syntax->list (syntax (term ... ...))))))
                         null)))
        #`(when #f
            (let ((bind void) ... (tmp void) ...)
              (void bound ... ... term-group ... start ... end ... prec ...))))))
  
  (define (build-parser filename src-pos suppress input-terms start end assocs prods)
    (let* ((grammar (parse-input input-terms start end assocs prods src-pos))
           (table (build-table grammar filename suppress))
           (num-non-terms (send grammar get-num-non-terms))
           (token-code
            `(let ((ht (make-hash-table)))
               (begin
                 ,@(map (lambda (term)
                          `(hash-table-put! ht 
                                            ',(gram-sym-symbol term)
                                            ,(+ num-non-terms (gram-sym-index term))))
                        (send grammar get-terms))
                 ht)))
           (actions-code
            `(vector ,@(map prod-action (send grammar get-prods)))))
    (values table
            token-code
            actions-code
            (fix-check-syntax input-terms start end assocs prods))))
      
  )
