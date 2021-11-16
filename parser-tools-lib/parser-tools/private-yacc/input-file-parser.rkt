#lang racket/base

  ;; routines for parsing the input to the parser generator and producing a
  ;; grammar (See grammar.rkt)
  
  (require "yacc-helper.rkt"
           "../private-lex/token-syntax.rkt"
           "grammar.rkt"
           racket/class
           racket/contract)
  (require (for-template racket/base))
  
(define (is-a-grammar%? x) (is-a? x grammar%))
  (provide/contract 
   (parse-input ((listof identifier?) (listof identifier?) (listof identifier?)
                 (or/c #f syntax?) syntax? any/c . -> . is-a-grammar%?))
   (get-term-list ((listof identifier?) . -> . (listof identifier?))))

  (define stx-for-original-property (read-syntax #f (open-input-string "original")))

  ;; get-args: ??? -> (values (listof syntax) (or/c #f (cons integer? stx)))
  (define (get-args i rhs src-pos term-defs)
    (let ((empty-table (make-hasheq))
          (biggest-pos #f))
      (hash-set! empty-table 'error #t)
      (for-each (lambda (td)
                  (let ((v (syntax-local-value td)))
                    (when (e-terminals-def? v)
                        (for-each (lambda (s)
                                    (hash-set! empty-table (syntax->datum s) #t))
                                  (syntax->list (e-terminals-def-t v))))))
                term-defs)
      (let ([args
             (let get-args ((i i) 
                            (rhs rhs))
               (cond
                 ((null? rhs) null)
                 (else
                  (let ((b (car rhs))
                        (name (if (hash-ref empty-table (syntax->datum (car rhs)) #f)
                                  (gensym)
                                  (string->symbol (format "$~a" i)))))
                    (cond
                      (src-pos
                       (let ([start-pos-id
                              (datum->syntax b (string->symbol (format "$~a-start-pos" i)) b stx-for-original-property)]
                             [end-pos-id
                              (datum->syntax b (string->symbol (format "$~a-end-pos" i)) b stx-for-original-property)])
                         (set! biggest-pos (cons start-pos-id end-pos-id))
                         `(,(datum->syntax b name b stx-for-original-property)
                           ,start-pos-id
                           ,end-pos-id
                           ,@(get-args (add1 i) (cdr rhs)))))
                      (else
                       `(,(datum->syntax b name b stx-for-original-property)
                         ,@(get-args (add1 i) (cdr rhs)))))))))])
        (values args biggest-pos))))
    
  ;; Given the list of terminal symbols and the precedence/associativity definitions,
  ;; builds terminal structures (See grammar.rkt)
  ;; build-terms: symbol list * symbol list list -> term list
  (define (build-terms term-list precs)
    (let ((counter 0)

	  ;;(term-list (cons (gensym) term-list))
          
          ;; Will map a terminal symbol to its precedence/associativity
          (prec-table (make-hasheq)))
      
      ;; Fill the prec table
      (for-each
       (lambda (p-decl)
	 (begin0
	  (let ((assoc (car p-decl)))
	    (for-each
	     (lambda (term-sym)
	       (hash-set! prec-table term-sym (make-prec counter assoc)))
	     (cdr p-decl)))
	  (set! counter (add1 counter))))
       precs)
      
      ;; Build the terminal structures
      (map 
       (lambda (term-sym)
         (make-term term-sym 
                    #f
                    (hash-ref prec-table term-sym #f)))
       term-list)))
  
  ;; Retrieves the terminal symbols from a terminals-def (See terminal-syntax.rkt)
  ;; get-terms-from-def: identifier? -> (listof identifier?)
  (define (get-terms-from-def term-syn)
    (let ((t (syntax-local-value term-syn (lambda () #f))))
      (cond
       ((terminals-def? t) (syntax->list (terminals-def-t t)))
       ((e-terminals-def? t) (syntax->list (e-terminals-def-t t)))
       (else
	(raise-syntax-error 
         'parser-tokens
         "undefined token group"
         term-syn)))))

  (define (get-term-list term-group-names)
    (remove-duplicates
     (cons (datum->syntax #f 'error)
           (apply append
                  (map get-terms-from-def term-group-names)))))
  
  (define (parse-input term-defs start ends prec-decls prods src-pos)
    (let* ((start-syms (map syntax-e start))
           
           (list-of-terms (map syntax-e (get-term-list term-defs)))

           (end-terms
            (map
             (lambda (end)
               (unless (memq (syntax-e end) list-of-terms)
                 (raise-syntax-error
                  'parser-end-tokens
                  (format "End token ~a not defined as a token"
                          (syntax-e end))
                  end))
               (syntax-e end))
             ends))
           
           ;; Get the list of terminals out of input-terms
           
           (list-of-non-terms
            (syntax-case prods ()
              (((non-term production ...) ...)
               (begin
                 (for-each
                  (lambda (nts)
                    (when (memq (syntax->datum nts) list-of-terms)
                        (raise-syntax-error
                         'parser-non-terminals
                         (format "~a used as both token and non-terminal"
                                 (syntax->datum nts))
                         nts)))
                  (syntax->list (syntax (non-term ...))))
                 
                 (let ((dup (duplicate-list? (syntax->datum 
                                              (syntax (non-term ...))))))
                   (when dup
                       (raise-syntax-error
                        'parser-non-terminals
                        (format "non-terminal ~a defined multiple times"
                                dup)
                        prods)))
                 
                 (syntax->datum (syntax (non-term ...)))))
              (_
               (raise-syntax-error
                'parser-grammar
                "Grammar must be of the form (grammar (non-terminal productions ...) ...)"
                prods))))
           
           ;; Check the precedence declarations for errors and turn them into data
           (precs
            (syntax-case prec-decls ()
              (((type term ...) ...)
               (let ((p-terms 
                      (syntax->datum (syntax (term ... ...)))))
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
                          (when (not (memq (syntax->datum t) 
                                         list-of-terms))
                              (raise-syntax-error
                               'parser-precedences
                               (format
                                "Precedence declared for non-token ~a"
                                (syntax->datum t))
                               t)))
                        (syntax->list a)))
                     (syntax->list (syntax ((term ...) ...))))
                    (for-each
                     (lambda (type)
                       (when (not (memq (syntax->datum type)
                                      `(left right nonassoc)))
                           (raise-syntax-error
                            'parser-precedences
                            "Associativity must be left, right or nonassoc"
                            type)))
                     (syntax->list (syntax (type ...))))
                    (syntax->datum prec-decls)))))
              (#f null)
              (_
               (raise-syntax-error
                'parser-precedences
                "Precedence declaration must be of the form (precs (assoc term ...) ...) where assoc is left, right or nonassoc"
                prec-decls))))
           
           (terms (build-terms list-of-terms precs))
           
           (non-terms (map (lambda (non-term) (make-non-term non-term #f))
                           list-of-non-terms))
           (term-table (make-hasheq))
           (non-term-table (make-hasheq)))
      
      (for-each (lambda (t)
		  (hash-set! term-table (gram-sym-symbol t) t))
		terms)
      
      (for-each (lambda (nt)
		  (hash-set! non-term-table (gram-sym-symbol nt) nt))
		non-terms)
      
      (let* (
	     ;; parse-prod: syntax -> gram-sym vector
	     (parse-prod
	      (lambda (prod-so)
                (syntax-case prod-so ()
                  ((prod-rhs-sym ...)
                   (andmap identifier? (syntax->list prod-so))
                   (begin
                     (for-each (lambda (t)
                                 (when (memq (syntax->datum t) end-terms)
                                     (raise-syntax-error
                                      'parser-production-rhs
                                      (format "~a is an end token and cannot be used in a production"
                                              (syntax->datum t))
                                      t)))
                               (syntax->list prod-so))
                     (list->vector
                      (map (lambda (s)
                             (hash-ref 
                              term-table 
                              (syntax->datum s)
                              (lambda ()
                                (hash-ref 
                                 non-term-table
                                 (syntax->datum s)
                                 (lambda ()
                                   (raise-syntax-error
                                    'parser-production-rhs
                                    (format 
                                     "~a is not declared as a terminal or non-terminal"
                                     (syntax->datum s))
                                    s))))))
                           (syntax->list prod-so)))))
                  (_
                   (raise-syntax-error
                    'parser-production-rhs
                    "production right-hand-side must have form (symbol ...)"
                    prod-so)))))
	     
             ;; parse-action: syntax * syntax -> syntax
             (parse-action 
              (lambda (rhs act)
                (let-values ([(args biggest) (get-args 1 (syntax->list rhs) src-pos term-defs)])
                  (let ([act 
                         (if biggest
                             (with-syntax ([$n-start-pos (datum->syntax (car biggest) '$n-start-pos)]
                                           [$n-end-pos (datum->syntax (cdr biggest) '$n-end-pos)])
                               #`(let ([$n-start-pos #,(car biggest)]
                                       [$n-end-pos #,(cdr biggest)])
                                   #,act))
                             act)])
                    (quasisyntax/loc act
                      (lambda #,args
                        #,act))))))
             
	     ;; parse-prod+action: non-term * syntax -> production
	     (parse-prod+action
	      (lambda (nt prod-so)
                (syntax-case prod-so ()
                  ((prod-rhs action)
                   (let ((p (parse-prod (syntax prod-rhs))))
                     (make-prod 
                      nt
                      p
                      #f
                      (let loop ((i (sub1 (vector-length p))))
                        (if (>= i 0)
                            (let ((gs (vector-ref p i)))
                              (if (term? gs)
                                  (term-prec gs)
                                  (loop (sub1 i))))
                            #f))
                      (parse-action (syntax prod-rhs) (syntax action)))))
                  ((prod-rhs (prec term) action)
                   (identifier? (syntax term))
                   (let ((p (parse-prod (syntax prod-rhs))))
                     (make-prod 
                      nt 
                      p
                      #f
                      (term-prec
                       (hash-ref 
                        term-table 
                        (syntax->datum (syntax term))
                        (lambda ()
                          (raise-syntax-error
                           'parser-production-rhs
                           (format
                            "unrecognized terminal ~a in precedence declaration"
                            (syntax->datum (syntax term)))
                           (syntax term)))))
                      (parse-action (syntax prod-rhs) (syntax action)))))
                  (_
                   (raise-syntax-error
                    'parser-production-rhs
                    "production must have form [(symbol ...) expression] or [(symbol ...) (prec symbol) expression]"
                    prod-so)))))
             
	     ;; parse-prod-for-nt: syntax -> production list
	     (parse-prods-for-nt
	      (lambda (prods-so)
                (syntax-case prods-so ()
                  ((nt productions ...)
                   (> (length (syntax->list (syntax (productions ...)))) 0)
                   (let ((nt (hash-ref non-term-table 
                                       (syntax->datum (syntax nt)))))
                     (map (lambda (p) (parse-prod+action nt p)) 
                          (syntax->list (syntax (productions ...))))))
		  (_
                   (raise-syntax-error
                    'parser-productions
                    "A production for a non-terminal must be (non-term right-hand-side ...) with at least 1 right hand side"
                    prods-so))))))
		 
        (for-each
         (lambda (sstx ssym)
           (unless (memq ssym list-of-non-terms)
             (raise-syntax-error
              'parser-start
              (format "Start symbol ~a not defined as a non-terminal" ssym)
              sstx)))
         start start-syms)
                 
        (let* ((starts (map (lambda (x) (make-non-term (gensym) #f)) start-syms))
               (end-non-terms (map (lambda (x) (make-non-term (gensym) #f)) start-syms))
               (parsed-prods (map parse-prods-for-nt (syntax->list prods)))
               (start-prods
                (map (lambda (start end-non-term)
                       (list (make-prod start (vector end-non-term) #f #f 
                                        (syntax (lambda (x) x)))))
                     starts end-non-terms))
               (prods 
                `(,@start-prods
                  ,@(map
                     (lambda (end-nt start-sym)
                       (map
                        (lambda (end)
                          (make-prod end-nt
                                     (vector
                                      (hash-ref non-term-table start-sym)
                                      (hash-ref term-table end))
                                     #f
                                     #f
                                     (syntax (lambda (x) x))))
                        end-terms))
                     end-non-terms start-syms)
                  ,@parsed-prods)))
          
          (make-object grammar%
            prods
            (map car start-prods)
            terms
            (append starts (append end-non-terms non-terms))
            (map (lambda (term-name)
                   (hash-ref term-table term-name))
                 end-terms))))))
