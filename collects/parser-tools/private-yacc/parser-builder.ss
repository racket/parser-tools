#cs
(module parser-builder mzscheme
  
  (require "input-file-parser.ss"
           "table.ss"
           "parser-actions.ss"
           "grammar.ss")
  
  (provide build-parser)
  
  (define (strip so)
    (syntax-local-introduce
     (datum->syntax-object
      #f
      (syntax-object->datum so)
      so
      so)))
  
  (define (fix-check-syntax start terms prods precs ends)
    (syntax-case prods ()
      ((_ (bind ((bound ...) x ...) ...) ...)
       (let ((binds (syntax->list (syntax (bind ...))))
             (bounds (cons start 
                           (apply 
                            append 
                            (map syntax->list 
                                 (apply 
                                  append 
                                  (map syntax->list 
                                       (syntax->list (syntax (((bound ...) ...) ...)))))))))
             (terms (get-term-list terms))
             (precs (if precs
                        (syntax-case precs ()
                          ((_ (__ term ...) ...)
                           (apply append (map syntax->list (syntax->list (syntax ((term ...) ...)))))))
                        null)))
         `(if #f (let ,(map (lambda (bind)
                              `(,(strip bind) void))
                            (append terms binds))
                   (void ,@(append ends precs (map strip bounds)))))))))
  
  (define (build-parser filename src-pos suppress input-terms start end assocs prods runtime)
    (let* ((grammar (parse-input start end input-terms assocs prods runtime src-pos))
	   (table (build-table grammar filename suppress))
           (table-code 
            `((lambda (table-list)
                (let ((v (list->vector table-list)))
                  (let build-table-loop ((i 0))
                    (cond
                      ((< i (vector-length v))
                       (let ((vi (vector-ref v i)))
                         (cond
                           ((list? vi) 
                            (vector-set! v i
                                         (cond
                                           ((eq? 's (car vi))
                                            (make-shift (cadr vi)))
                                           ((eq? 'r (car vi))
                                            (make-reduce (cadr vi) (caddr vi) (cadddr vi)))
                                           ((eq? 'a (car vi)) (make-accept)))))))
                       (build-table-loop (add1 i)))
                      (else v)))))
              (quote
               ,(map (lambda (action)
                       (cond
                         ((shift? action)
                          `(s ,(shift-state action)))
                         ((reduce? action)
                          `(r ,(reduce-prod-num action)
                              ,(reduce-lhs-num action)
                              ,(reduce-rhs-length action)))
                         ((accept? action)
                          `(a))
                         (else action)))
                     (vector->list table)))))
            
           (num-non-terms (length (grammar-non-terms grammar)))

           (token-code
            `(let ((ht (make-hash-table)))
               (begin
                 ,@(map (lambda (term)
                          `(hash-table-put! ht 
                                            ',(gram-sym-symbol term)
                                            ,(+ num-non-terms (gram-sym-index term))))
                        (grammar-terms grammar))
                 ht)))
           
           (actions-code
            `(vector ,@(map prod-action (grammar-prods grammar)))))
      (values table-code
              token-code
              actions-code
              (fix-check-syntax start input-terms prods assocs end))))
      
  )
