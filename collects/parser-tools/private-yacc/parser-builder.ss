#cs
(module parser-builder mzscheme
  
  (require "input-file-parser.ss"
           "table.ss"
           "parser-actions.ss"
           "grammar.ss")
  
  (provide build-parser)
  
  (define (fix-check-syntax start terms prods)
    (syntax-case prods ()
      ((_ (bind ((bound ...) x ...) ...) ...)
       (let ((binds (syntax->list (syntax (bind ...))))
             (bounds (cons start 
                           (apply append (map syntax->list 
                                              (apply append (map syntax->list 
                                                                 (syntax->list (syntax (((bound ...) ...) ...)))))))))
             (terms (get-term-list terms)))
         `(if #f (let ,(map (lambda (bind)
                              `(,bind void))
                            (append terms binds))
                   (void ,@bounds)))))))
  
  (define (build-parser filename src-pos suppress error-expr input-terms start end assocs prods runtime src)
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
            `(vector ,@(map prod-action (grammar-prods grammar))))
           
           (parser-code
            `(letrec ((err ,error-expr)
                      (err-state 0)
                      (table ,table-code)
                      (term-sym->index ,token-code)
                      (actions ,actions-code)
                      (reduce-stack
                       (lambda (s n v)
                         (if (> n 0)
                             ,(if src-pos
                                  `(reduce-stack (cddddr s) (sub1 n) `(,(cadr s) ,(caddr s) ,(cadddr s) ,@v))
                                  `(reduce-stack (cddr s) (sub1 n) (cons (cadr s) v)))
                             (values s v))))
                      (fix-error
                       (lambda (stack ip get-token)
                         (letrec ((remove-input
				   (lambda ()
				     (let ((a (find-action stack ip)))
				       (cond
					((shift? a)
					 ;; (printf "shift:~a~n" (shift-state a))
					 (cons (shift-state a)
					       (cons (if (token? ip)
							 (token-value ip)
							 #f)
						     stack)))
					(else
					 (printf "discard-input:~a~n" (if (token? ip)
									  (token-name ip)
									  ip))
					 (set! ip (get-token))
					 (remove-input))))))
				  (remove-states
				   (lambda ()
				     (let ((a (find-action stack 'error)))
				       (cond
					((shift? a)
					 ;; (printf "shift:~a~n" (shift-state a))
					 (set! stack (cons (shift-state a) (cons #f stack)))
					 (remove-input))				      
					(else
					 ;; (printf "discard-state:~a~n" (car stack))
					 (cond
					  ((< (length stack) 3)
					   (printf "Unable to shift error token~n")
					   #f)					
					  (else
					   (set! stack (cddr stack))
					   (remove-states)))))))))
			   (remove-states))))
                      
                      (find-action
                       (lambda (stack tok ,@(if src-pos `(ip) `()))
                         (array2d-ref table 
                                      (car stack)
                                      (hash-table-get term-sym->index
                                                      (token-name tok)
                                                      (lambda ()
                                                        ,(if src-pos
                                                             `(err #t (token-name tok) (token-value tok) (cadr ip) (caddr ip))
                                                             `(err #t (token-name tok) (token-value tok)))
							(raise-read-error (format "parser: got token of unknown type ~a" (token-name tok)))))))))
								
               (lambda (get-token)
                 (let parsing-loop ((stack (list 0))
				    (ip (get-token)))
                   (let* ((tok ,(if src-pos `(cond
                                               ((and (list? ip) (= 3 (length ip)))
                                                (let ((tok (car ip)))
                                                  (cond
                                                    ((symbol? tok) (make-token tok #f))
                                                    ((token? tok) tok)
                                                    (else (raise-type-error 'parser 
                                                                            "(list (token or symbol) position position)"
                                                                            0 
                                                                            ip)))))
                                               (else
                                                (raise-type-error 'parser 
                                                                  "(list (token or symbol) position position)"
                                                                  0
                                                                  ip)))
                                                `(cond
                                                   ((symbol? ip) (make-token ip #f))
                                                   ((token? ip) ip)
                                                   (else (raise-type-error 'parser "token or symbol" 0 ip)))))
                          (action (find-action stack tok ,@(if src-pos `(ip) `()))))
                     (cond
                       ((shift? action)
                        ;; (printf "shift:~a~n" (shift-state action))
                        (let ((val (token-value tok)))
                          (parsing-loop ,(if src-pos
                                             ``(,(shift-state action) ,val ,(cadr ip) ,(caddr ip) ,@stack)
                                             ``(,(shift-state action) ,val ,@stack))
                                        (get-token))))
                       ((reduce? action)
                        ;; (printf "reduce:~a~n" (reduce-prod-num action))
                        (let-values (((new-stack args)
                                      (reduce-stack stack 
                                                    (reduce-rhs-length action)
                                                    null)))
                          (let* ((A (reduce-lhs-num action))
                                 (goto (array2d-ref table (car new-stack) A)))
                            (parsing-loop ,(if src-pos
                                               ``(,goto 
                                                  ,(apply
                                                    (vector-ref actions
                                                                (reduce-prod-num action))
                                                    args)
                                                  ,(if (null? args)
                                                       (cadr ip)
                                                       (cadr args))
                                                  ,(if (null? args)
                                                       (cadr ip)
                                                       (list-ref args (- (* (reduce-rhs-length action) 3) 1)))
                                                  ,@new-stack)
                                               ``(,goto 
                                                  ,(apply 
                                                    (vector-ref actions 
                                                                (reduce-prod-num action))
                                                    args)
                                                  ,@new-stack))
                                          ip))))
                       ((accept? action)
                        ;; (printf "accept~n")
                        (cadr stack))
                       (else 
                        ,(if src-pos
                             `(err #t (token-name tok) (token-value tok) (cadr ip) (caddr ip))
                             `(err #t (token-name tok) (token-value tok)))
                        (let ((new-stack (fix-error stack ip get-token)))
                          (if new-stack
                              (parsing-loop new-stack (get-token))
                              (raise-read-error 
                               "parser: Could not parse input"
                               #f #f #f #f #f)))))))))))
      (datum->syntax-object
       runtime
       `(begin #|,(fix-check-syntax start input-terms prods)|# ,parser-code)
       src))))
