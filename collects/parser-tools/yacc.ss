#cs
(module yacc mzscheme
  
  (require-for-syntax "private-yacc/parser-builder.ss"
                      "private-yacc/yacc-helper.ss")
  (require "private-yacc/parser-actions.ss"
           "private-yacc/array2d.ss"
           "private-lex/token.ss"
	   (lib "readerr.ss" "syntax"))
  
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
         (let-values (((table term-sym->index actions check-syntax-fix)
                       (build-parser (if debug debug "")
                                     src-pos
                                     suppress
                                     tokens
                                     start
                                     end
                                     precs
                                     grammar
                                     stx)))
           (with-syntax ((check-syntax-fix check-syntax-fix)
                         (err error)
                         (ends end)
                         (table table)
                         (term-sym->index term-sym->index)
                         (actions actions)
                         (src-pos src-pos))
             (syntax
              (begin
                check-syntax-fix
                (parser-body err (quote ends) table term-sym->index actions src-pos)))))))
      (_
       (raise-syntax-error
        #f
        "parser must have the form (parser args ...)"
        stx))))

  (define (reduce-stack stack num ret-vals src-pos)
    (cond
      ((> num 0)
       (let* ((top-frame (car stack))
              (ret-vals
               (if src-pos
                   (cons (stack-frame-value top-frame)
                         (cons (stack-frame-start-pos top-frame)
                               (cons (stack-frame-end-pos top-frame)
                                     ret-vals)))
                   (cons (stack-frame-value top-frame) ret-vals))))
         (reduce-stack (cdr stack) (sub1 num) ret-vals src-pos)))
      (else (values stack ret-vals))))

  (define-struct stack-frame (state value start-pos end-pos))
 
  (define empty-stack (list (make-stack-frame 0 #f #f #f)))

  (define (false-thunk) #f) 

  (define (parser-body err ends table term-sym->index actions src-pos)
    (letrec ((input->token
              (if src-pos
                  (lambda (ip)
                    (cond
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
                                         ip))))
                  (lambda (ip)
                    (cond
                      ((symbol? ip) (make-token ip #f))
                      ((token? ip) ip)
                      (else (raise-type-error 'parser "token or symbol" 0 ip))))))
             
             (fix-error
              (lambda (stack tok ip get-token)
                (letrec ((remove-input
                          (lambda ()
                            (if (memq (token-name tok) ends)
                                #f
                                (let ((a (find-action stack tok ip)))
                                  (cond
                                    ((shift? a)
                                     ;; (printf "shift:~a~n" (shift-state a))
                                     (cons (if src-pos
                                               (make-stack-frame (shift-state a)
                                                                 (if (token? ip) (token-value ip) #f)
                                                                 (cadr ip)
                                                                 (caddr ip))
                                               (make-stack-frame (shift-state a)
                                                                 (if (token? ip) (token-value ip) #f)
                                                                 #f
                                                                 #f))
                                           stack))
                                    (else
                                     ;; (printf "discard input:~a~n" tok)
                                     (set! ip (get-token))
                                     (set! tok (input->token ip))
                                     (remove-input)))))))
                         (remove-states
                          (lambda ()
                            (let ((a (find-action stack (make-token 'error #f) ip)))
                              (cond
                                ((shift? a)
                                 ;; (printf "shift:~a~n" (shift-state a))
                                 (set! stack 
                                       (cons
                                        (if src-pos
                                           (make-stack-frame (shift-state a) 
                                                             #f 
                                                             (cadr ip)
                                                             (caddr ip))
                                           (make-stack-frame (shift-state a) 
                                                             #f
                                                             #f
                                                             #f))
                                        stack))
                                 (remove-input))
                                (else
                                 ;; (printf "discard state:~a~n" (car stack))
                                 (cond
                                   ((< (length stack) 2)
                                    (printf "Unable to shift error token~n")
                                    #f)					
                                   (else
                                    (set! stack (cdr stack))))))))))
                  (remove-states))))
             
             (find-action
              (lambda (stack tok ip)
                (let ((token-index (hash-table-get term-sym->index
                                                   (token-name tok)
                                                   false-thunk)))
                  (if token-index
                      (array2d-ref table 
                                   (stack-frame-state (car stack))
                                   token-index)
                      (begin
                        (if src-pos
                            (err #t (token-name tok) (token-value tok) (cadr ip) (caddr ip))
                            (err #t (token-name tok) (token-value tok)))
                        (raise-read-error (format "parser: got token of unknown type ~a" (token-name tok))
                                          #f #f #f #f #f)))))))
      (lambda (get-token)
        (let parsing-loop ((stack empty-stack)
                           (ip (get-token)))
          (let* ((tok (input->token ip))
                 (action (find-action stack tok ip)))
            (cond
              ((shift? action)
               ;; (printf "shift:~a~n" (shift-state action))
               (let ((val (token-value tok)))
                 (parsing-loop (cons (if src-pos
                                         (make-stack-frame (shift-state action)
                                                           val
                                                           (cadr ip)
                                                           (caddr ip))
                                         (make-stack-frame (shift-state action)
                                                           val
                                                           #f
                                                           #f))
                                     stack)
                               (get-token))))
              ((reduce? action)
               ;; (printf "reduce:~a~n" (reduce-prod-num action))
               (let-values (((new-stack args)
                             (reduce-stack stack 
                                           (reduce-rhs-length action)
                                           null
                                           src-pos)))
                 (let* ((A (reduce-lhs-num action))
                        (goto (array2d-ref table (stack-frame-state (car new-stack)) A)))
                   (parsing-loop (cons
                                  (if src-pos
                                      (make-stack-frame goto 
                                                        (apply (vector-ref actions (reduce-prod-num action)) args)
                                                        (if (null? args) (cadr ip) (cadr args))
                                                        (if (null? args) 
                                                            (caddr ip)
                                                            (list-ref args (- (* (reduce-rhs-length action) 3) 1))))
                                      (make-stack-frame goto 
                                                        (apply (vector-ref actions (reduce-prod-num action)) args)
                                                        #f
                                                        #f))
                                  new-stack)
                                 ip))))
              ((accept? action)
               ;; (printf "accept~n")
               (stack-frame-value (car stack)))
              (else 
               (if src-pos
                   (err #t (token-name tok) (token-value tok) (cadr ip) (caddr ip))
                   (err #t (token-name tok) (token-value tok)))
               (let ((new-stack (fix-error stack tok ip get-token)))
                 (if new-stack
                     (parsing-loop new-stack (get-token))
                     (raise-read-error 
                      "parser: Could not parse input"
                      #f #f #f #f #f))))))))))
  
  )