(module parser-builder mzscheme
  
  (require "input-file-parser.ss"
           "grammar.ss"
           "table.ss"
           (lib "class.ss")
           (lib "contract.ss"))
  
  (provide/contract
   (build-parser ((string? any? any? syntax? (listof syntax?) (listof syntax?)
                   (union syntax? false?) syntax?) . ->* . (any? any? any? any?))))
  
  (define (strip so)
    (syntax-local-introduce
     (datum->syntax-object
      #f
      (syntax-object->datum so)
      so
      so)))
  
  (define (fix-check-syntax start term-groups prods precs ends)
    (syntax-case prods ()
      ((_ (bind ((bound ...) x ...) ...) ...)
       (let ((binds (syntax->list (syntax (bind ...))))
             (bounds (append start 
                             (apply 
                              append 
                              (map syntax->list 
                                   (apply 
                                    append 
                                    (map syntax->list 
                                         (syntax->list (syntax (((bound ...) ...) ...)))))))))
             (terms (get-term-list term-groups))
             (term-group-stx
              (map (lambda (tg)
                     (syntax-property
                      (datum->syntax-object tg #f)
                      'disappeared-use
                      tg))
                   (syntax->list term-groups)))
             (precs (if precs
                        (syntax-case precs ()
                          ((_ (__ term ...) ...)
                           (apply append (map syntax->list (syntax->list (syntax ((term ...) ...)))))))
                        null)))
         `(if #f (let ,(map (lambda (bind)
                              `(,(strip bind) void))
                            (append terms binds))
                   (void ,@(append ends precs term-group-stx (map strip bounds)))))))))
  
  (define (build-parser filename src-pos suppress input-terms start end assocs prods)
    (let* ((grammar (parse-input start end input-terms assocs prods src-pos))
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
            (fix-check-syntax start input-terms prods assocs end))))
      
  )
