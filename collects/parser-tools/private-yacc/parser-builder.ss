#cs
(module parser-builder mzscheme
  
  (require "input-file-parser.ss"
           "table.ss"
           "parser-actions.ss"
           "grammar.ss"
           (lib "pretty.ss"))
  
  (provide build-parser)
  
  (define (build-parser start input-terms assocs prods filename runtime src)
    (let* ((grammar (parse-input start input-terms assocs prods))
           (table (build-table grammar ""))
           (table-code 
            (cons 'vector 
                  (map (lambda (action)
                         (cond
                           ((shift? action)
                            `(make-shift ,(shift-state action)))
                           ((reduce? action)
                            `(make-reduce ,(reduce-prod-num action)
                                          ,(reduce-lhs-num action)
                                          ,(reduce-rhs-length action)))
                           ((accept? action)
                            `(make-accept))
                           (else action)))
                       (vector->list table))))
            
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
           
           (parser-code
            `(letrec ((term-sym->index ,token-code)
                      (table ,table-code)
                      (pop-2x
                       (lambda (s n)
                         (if (> n 0)
                             (pop-2x (cdr (cdr s)) (sub1 n))
                             s))))
               (lambda (get-token)
                 (let loop ((stack (list 0)))
                   (let* ((next (get-token))
                          (s (car stack))
                          (a (hash-table-get term-sym->index 
                                             (if (token? next)
                                                 (token-name next)
                                                 next)))
                          (action (array2d-ref table s a)))
                     (cond
                       ((shift? action)
                        (loop (cons (shift-state action) (cons a stack))))
                       ((reduce? action)
                        (display (reduce-prod-num action))
                        (newline)
                        (let* ((A (reduce-lhs-num action))
                               (new-stack (pop-2x stack (reduce-rhs-length action)))
                               (goto (array2d-ref table (car new-stack) A)))
                          (loop (cons goto (cons A new-stack)))))
                       ((accept? action)
                        (printf "accept~n")))))))))
      (pretty-print parser-code)
      (newline)
      (datum->syntax-object
       runtime
       parser-code
       src))))
           