#cs
(module input-file-parser mzscheme

  ;; routines for parsing the input to the parser generator and producing a
  ;; grammar (See grammar.ss)
  
  (require "yacc-helper.ss" "terminal-syntax.ss" "grammar.ss" (lib "list.ss"))

  (provide parse-input)

  ;; get-args: num * syntax-object -> syntax-object list
  (define (get-args x act)
    (let loop ((i 1))
      (cond
        ((> i x) null)
        (else (cons (datum->syntax-object act (string->symbol (format "$~a" i)))
                    (loop (add1 i)))))))
    
  ;; nullable: production list * int -> non-term set
  ;; determines which non-terminals can derive epsilon
  (define (nullable prods num-nts)
    (letrec ((nullable (make-vector num-nts #f))
	     (added #f)
	     
	     ;; possible-nullable: producion list -> production list
	     ;; Removes all productions that have a terminal
	     (possible-nullable
	      (lambda (prods)
		(filter (lambda (prod)
			  (vector-andmap non-term? (prod-rhs prod)))
			prods)))
	     
	     ;; set-nullables: production list -> production list
	     ;; makes one pass through the productions, adding the ones
	     ;; known to be nullable now to nullable and returning a list
	     ;; of productions that we don't know about yet. 
	     (set-nullables
	      (lambda (prods)
		(cond
		 ((null? prods) null)
		 ((vector-ref  nullable 
			       (gram-sym-index (prod-lhs (car prods))))
		  (set-nullables (cdr prods)))
		 ((vector-andmap (lambda (nt) 
				   (vector-ref nullable (gram-sym-index nt)))
				 (prod-rhs (car prods)))
		  (vector-set! nullable 
			       (gram-sym-index (prod-lhs (car prods)))
			       #t)
		  (set! added #t)
		  (set-nullables (cdr prods)))
		 (else
		  (cons (car prods) 
			(set-nullables (cdr prods))))))))
      
      (let loop ((P (possible-nullable prods)))
	(cond
	 ((null? P) nullable)
	 (else
	  (set! added #f)
	  (let ((new-P (set-nullables P)))
	    (if added
		(loop new-P)
		nullable)))))))

  
  ;; Given the list of terminal symbols and the precedence/associativity definitions,
  ;; builds terminal structures (See grammar.ss)
  ;; build-terms: symbol list * symbol list list -> term list
  (define (build-terms term-list precs)
    (let ((counter 0)

	  ;;(term-list (cons (gensym) term-list))
          
          ;; Will map a terminal symbol to its precedence/associativity
          (prec-table (make-hash-table)))
      
      ;; Fill the prec table
      (for-each
       (lambda (p-decl)
	 (begin0
	  (let ((assoc (car p-decl)))
	    (for-each
	     (lambda (term-sym)
	       (hash-table-put! prec-table term-sym (make-prec counter assoc)))
	     (cdr p-decl)))
	  (set! counter (add1 counter))))
       precs)
      
      (set! counter 0)

      ;; Build the terminal structures
      (map 
       (lambda (term-sym)
	 (begin0
	  (make-term term-sym 
		     counter 
		     (hash-table-get prec-table term-sym (lambda () #f)))
	  (set! counter (add1 counter))))
       term-list)))
  
  ;; Retrieves the terminal symbols from a terminals-def (See terminal-syntax.xx)
  ;; get-terms-from-def: syntax-object -> symbol list
  (define (get-terms-from-def term-syn)
    (let ((t (syntax-local-value term-syn (lambda () #f))))
      (cond
       ((terminals-def? t) (terminals-def-t t))
       (else
	(raise-syntax-error 
         'parser-tokens
         "undefined token group"
         term-syn)))))
  
  ;; parse-input: syntax-object * syntax-object list * syntax-object^4 -> grammar
  (define (parse-input start ends term-defs prec-decls prods runtime)
    (let* ((counter 0)
           
           (start-sym (syntax-object->datum start))
                          
           
           (list-of-terms
            (syntax-case term-defs (tokens)
              ((tokens term-def ...)
               (andmap identifier? (syntax->list (syntax (term-def ...))))
               (remove-duplicates
                (cons 'error
                      (apply append
                             (map get-terms-from-def 
                                  (syntax->list (syntax (term-def ...))))))))
              (_
               (raise-syntax-error
                'parser-tokens
                "Token declaration must be (tokens symbol ...)"
                term-defs))))

           (end-terms
            (map
             (lambda (end)
               (if (not (memq (syntax-object->datum end) list-of-terms))
                   (raise-syntax-error
                    'parser-end-tokens
                    (format "End token ~a not defined as a token"
                            (syntax-object->datum end))
                    end)
                   (syntax-object->datum end)))
             ends))
           
           ;; Get the list of terminals out of input-terms
           
           (list-of-non-terms
            (syntax-case prods (grammar)
              ((grammar (non-term production ...) ...)
               (begin
                 (for-each
                  (lambda (nts)
                    (if (memq (syntax-object->datum nts) list-of-terms)
                        (raise-syntax-error
                         'parser-non-terminals
                         (format "~a used as both token and non-terminal"
                                 (syntax-object->datum nts))
                         nts)))
                  (syntax->list (syntax (non-term ...))))
                 
                 (if (not (memq start-sym
                                (syntax-object->datum (syntax (non-term ...)))))
                     (raise-syntax-error
                      'parser-start
                      (format "Start symbol ~a not defined as a non-terminal"
                              start-sym)
                      start))
                 
                 (let ((dup (duplicate-list? (syntax-object->datum 
                                              (syntax (non-term ...))))))
                   (if dup
                       (raise-syntax-error
                        'parser-non-terminals
                        (format "non-terminal ~a defined multiple times"
                                dup)
                        prods)))
                 
                 (syntax-object->datum (syntax (non-term ...)))))
              (_
               (raise-syntax-error
                'parser-grammar
                "Grammar must be of the form (grammar (non-terminal productions ...) ...)"
                prods))))
           
           ;; Check the precedence declarations for errors and turn them into data
           (precs
            (syntax-case prec-decls (precs)
              ((precs (type term ...) ...)
               (let ((p-terms 
                      (apply append (syntax-object->datum 
                                     (syntax ((term ...) ...))))))
                 (cond
                   ((duplicate-list? p-terms) =>
                    (lambda (d)
                      (raise-syntax-error
                       'parser-precedences
                       (format "duplicate precedence declaration for token ~a"
                               d)
                       prec-decls)))
                   (else
                    (for-each
                     (lambda (a)
                       (for-each
                        (lambda (t)
                          (if (not (memq (syntax-object->datum t) 
                                         list-of-terms))
                              (raise-syntax-error
                               'parser-precedences
                               (format
                                "Precedence declared for non-token ~a"
                                (syntax-object->datum t))
                               t)))
                        (syntax->list a)))
                     (syntax->list (syntax ((term ...) ...))))
                    (for-each
                     (lambda (type)
                       (if (not (memq (syntax-object->datum type)
                                      `(left right nonassoc)))
                           (raise-syntax-error
                            'parser-precedences
                            "Associativity must be left, right or nonassoc"
                            type)))
                     (syntax->list (syntax (type ...))))
                    (cdr (syntax-object->datum prec-decls))))))
              (#f null)
              (_
               (raise-syntax-error
                'parser-precedences
                "Precedence declaration must be of the form (precs (assoc term ...) ...) where assoc is left, right or nonassoc"
                prec-decls))))
           
           (terms (build-terms list-of-terms precs))
           
           (non-terms (begin
                        (set! counter 1)
                        (map (lambda (non-term)
                               (begin0
                                 (make-non-term non-term counter)
                                 (set! counter (add1 counter))))
                             list-of-non-terms)))
           (term-table (make-hash-table))
           (non-term-table (make-hash-table)))
      
      (for-each (lambda (t)
		  (hash-table-put! term-table (gram-sym-symbol t) t))
		terms)
      
      (for-each (lambda (nt)
		  (hash-table-put! non-term-table (gram-sym-symbol nt) nt))
		non-terms)
      
      (set! counter 1)
      
      (let* (
	     ;; parse-prod: syntax-object -> gram-sym vector
	     (parse-prod
	      (lambda (prod-so)
                (syntax-case prod-so ()
                  ((prod-rhs-sym ...)
                   (andmap identifier? (syntax->list prod-so))
                   (list->vector
                    (map (lambda (s)
                           (hash-table-get 
                            term-table 
                            (syntax-object->datum s)
                            (lambda ()
                              (hash-table-get 
                               non-term-table
                               (syntax-object->datum s)
                               (lambda ()
                                 (raise-syntax-error
                                  'parser-production-rhs
                                  (format 
                                   "~a is not declared as a terminal or non-terminal"
                                   (syntax-object->datum s))
                                  s))))))
                         (syntax->list prod-so))))
                  (_
                   (raise-syntax-error
                    'parser-production-rhs
                    "production right-hand-side must have form (symbol ...)"
                    prod-so)))))
	     
             ;; parse-action: gram-sym vector * syntax-object -> syntax-object
             (parse-action 
              (lambda (prod act)
                (datum->syntax-object
                 runtime
                 `(lambda ,(get-args (vector-length prod) act)
                    ,act)
                 act)))
             
	     ;; parse-prod+action: non-term * syntax-object -> production
	     (parse-prod+action
	      (lambda (nt prod-so)
                (syntax-case prod-so (prec)
                  ((prod-rhs action)
                   (let ((p (parse-prod (syntax prod-rhs))))
                     (begin0
                       (make-prod 
                        nt 
                        p
                        counter 
                        (let loop ((i (sub1 (vector-length p))))
                          (if (>= i 0)
                              (let ((gs (vector-ref p i)))
                                (if (term? gs)
                                    (term-prec gs)
                                    (loop (sub1 i))))
                              #f))
                        (parse-action p (syntax action)))
                       (set! counter (add1 counter)))))
                  ((prod-rhs (prec term) action)
                   (identifier? (syntax term))
                   (let ((p (parse-prod (syntax prod-rhs))))
                     (begin0
                       (make-prod 
                        nt 
                        p
                        counter
                        (term-prec
                         (hash-table-get 
                          term-table 
                          (syntax-object->datum (syntax term))
                          (lambda ()
                            (raise-syntax-error
                             'parser-production-rhs
                             (format
                              "unrecognized terminal ~a in precedence declaration"
                              (syntax-object->datum (syntax term)))
                             (syntax term)))))
                        (parse-action p (syntax action)))
                       (set! counter (add1 counter)))))
                  (_
                   (raise-syntax-error
                    'parser-production-rhs
                    "production must have form [(symbol ...) expression] or [(symbol ...) (prec symbol) expression]"
                    prod-so)))))
             
	     ;; parse-prod-for-nt: syntax-object -> production list
	     (parse-prods-for-nt
	      (lambda (prods-so)
                (syntax-case prods-so ()
                  ((nt productions ...)
                   (> (length (syntax->list (syntax (productions ...)))) 0)
                   (let* ((prods (syntax-e prods-so))
                          (nt (hash-table-get non-term-table 
                                              (syntax-e (car prods)))))
                     (map (lambda (p) (parse-prod+action nt p)) 
                          (cdr prods))))
                  (_
                   (raise-syntax-error
                    'parser-productions
                    "A production for a non-terminal must be (non-term right-hand-side ...) with at least 1 right hand side"
                    prods-so))))))
	
        (let* ((start (make-non-term 'Start 0))
               (prods 
                (cons
                 (list (make-prod start
                                  (vector (hash-table-get non-term-table start-sym)
                                          (hash-table-get term-table (car end-terms)))
                                  0
                                  #f
                                  (datum->syntax-object
                                   runtime
                                   `(lambda (x) x))))
                 (map parse-prods-for-nt (cdr (syntax->list prods)))))
               (nulls (nullable (apply append prods) 
                                (add1 (length non-terms)))))
          
          
;          (printf "nullable: {~a}~n~n"
;                  (apply string-append 
;                         (let loop ((i 0))
;                           (cond
;                             ((>= i (vector-length nulls)) null)
;                             ((vector-ref nulls i)
;                              (cons
;                               (format "~a" 
;                                       (gram-sym-symbol
;                                        (list-ref (cons start non-terms) i)))
;                               (loop (add1 i))))
;                             (else (loop (add1 i)))))))
          (make-grammar
           (list->vector prods)
           (apply append prods)
           nulls
           (cons start non-terms)
           terms
           counter
           end-terms))))))
