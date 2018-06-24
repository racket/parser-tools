#lang racket/base  
(require "yacc-helper.rkt"
         "../private-lex/token-syntax.rkt"
         "grammar.rkt"
         racket/class
         racket/contract
         (for-template racket/base))

;; routines for parsing the input to the parser generator and producing a
;; grammar (See grammar.rkt)

(define (is-a-grammar%? x) (is-a? x grammar%))
(provide/contract 
 [parse-input ((listof identifier?) (listof identifier?) (listof identifier?)
                                    (or/c #f syntax?) syntax? any/c . -> . is-a-grammar%?)]
 [get-term-list ((listof identifier?) . -> . (listof identifier?))])

(define stx-for-original-property (read-syntax #f (open-input-string "original")))

;; get-args: ??? -> (values (listof syntax) (or/c #f (cons integer? stx)))
(define (get-args i rhs src-pos term-defs)
  (define empty-table (make-hasheq))
  (define biggest-pos #f)
  (hash-set! empty-table 'error #t)
  (for* ([td (in-list term-defs)]
         [v (in-value (syntax-local-value td))]
         #:when (e-terminals-def? v)
         [s (in-list (syntax->list (e-terminals-def-t v)))])
        (hash-set! empty-table (syntax->datum s) #t))
  (define args
    (let get-args ([i i][rhs rhs])
      (cond
        [(null? rhs) null]
        [else
         (define b (car rhs))
         (define name (if (hash-ref empty-table (syntax->datum (car rhs)) #f)
                          (gensym)
                          (string->symbol (format "$~a" i))))
         (cond
           [src-pos
            (define start-pos-id
              (datum->syntax b (string->symbol (format "$~a-start-pos" i)) b stx-for-original-property))
            (define end-pos-id
              (datum->syntax b (string->symbol (format "$~a-end-pos" i)) b stx-for-original-property))
            (set! biggest-pos (cons start-pos-id end-pos-id))
            (list* (datum->syntax b name b stx-for-original-property)
                   start-pos-id
                   end-pos-id
                   (get-args (add1 i) (cdr rhs)))]
           [else
            (list* (datum->syntax b name b stx-for-original-property)
                   (get-args (add1 i) (cdr rhs)))])])))
  (values args biggest-pos))
    
;; Given the list of terminal symbols and the precedence/associativity definitions,
;; builds terminal structures (See grammar.rkt)
;; build-terms: symbol list * symbol list list -> term list
(define (build-terms term-list precs)
  (define counter 0)
  ;;(term-list (cons (gensym) term-list))
  ;; Will map a terminal symbol to its precedence/associativity
  (define prec-table (make-hasheq))
    
  ;; Fill the prec table
  (for ([p-decl (in-list precs)])
       (define assoc (car p-decl))
       (for ([term-sym (in-list (cdr p-decl))])
            (hash-set! prec-table term-sym (make-prec counter assoc)))
       (set! counter (add1 counter)))
      
  ;; Build the terminal structures
  (for/list ([term-sym (in-list term-list)])
            (make-term term-sym 
                       #f
                       (hash-ref prec-table term-sym (λ () #f)))))
  
;; Retrieves the terminal symbols from a terminals-def (See terminal-syntax.rkt)
;; get-terms-from-def: identifier? -> (listof identifier?)
(define (get-terms-from-def term-syn)
  (define t (syntax-local-value term-syn #f))
  (cond
    [(terminals-def? t) (syntax->list (terminals-def-t t))]
    [(e-terminals-def? t) (syntax->list (e-terminals-def-t t))]
    [else
     (raise-syntax-error 
      'parser-tokens
      "undefined token group"
      term-syn)]))

(define (get-term-list term-group-names)
  (remove-duplicates
   (cons (datum->syntax #f 'error)
         (apply append (map get-terms-from-def term-group-names)))))
  
(define (parse-input term-defs start ends prec-decls prods src-pos)
  (define start-syms (map syntax-e start))
  (define list-of-terms (map syntax-e (get-term-list term-defs)))
  (define end-terms
    (for/list ([end (in-list ends)])
              (unless (memq (syntax-e end) list-of-terms)
                (raise-syntax-error
                 'parser-end-tokens
                 (format "End token ~a not defined as a token"
                         (syntax-e end))
                 end))
              (syntax-e end)))
  ;; Get the list of terminals out of input-terms
  (define list-of-non-terms
    (syntax-case prods ()
      [((NON-TERM PRODUCTION ...) ...)
       (begin
         (for ([nts (in-list (syntax->list #'(NON-TERM ...)))]
               #:when (memq (syntax->datum nts) list-of-terms))
              (raise-syntax-error
               'parser-non-terminals
               (format "~a used as both token and non-terminal" (syntax->datum nts))
               nts))
         (let ([dup (duplicate-list? (syntax->datum #'(NON-TERM ...)))])
           (when dup
             (raise-syntax-error
              'parser-non-terminals
              (format "non-terminal ~a defined multiple times" dup)
              prods)))
         (syntax->datum #'(NON-TERM ...)))]
      [_ (raise-syntax-error
          'parser-grammar
          "Grammar must be of the form (grammar (non-terminal productions ...) ...)"
          prods)]))
  ;; Check the precedence declarations for errors and turn them into data
  (define precs
    (syntax-case prec-decls ()
      [((TYPE TERM ...) ...)
       (let ([p-terms (syntax->datum #'(TERM ... ...))])
         (cond
           [(duplicate-list? p-terms) =>
                                      (λ (d)
                                        (raise-syntax-error
                                         'parser-precedences
                                         (format "duplicate precedence declaration for token ~a" d)
                                         prec-decls))]
           [else (for ([t (in-list (syntax->list #'(TERM ... ...)))]
                       #:when (not (memq (syntax->datum t) list-of-terms)))
                      (raise-syntax-error
                       'parser-precedences
                       (format "Precedence declared for non-token ~a" (syntax->datum t))
                       t))
                 (for ([type (in-list (syntax->list #'(TYPE ...)))]
                       #:unless (memq (syntax->datum type) `(left right nonassoc)))
                      (raise-syntax-error
                       'parser-precedences
                       "Associativity must be left, right or nonassoc"
                       type))
                 (syntax->datum prec-decls)]))]
      [#f null]
      [_ (raise-syntax-error
          'parser-precedences
          "Precedence declaration must be of the form (precs (assoc term ...) ...) where assoc is left, right or nonassoc"
          prec-decls)]))

  (define terms (build-terms list-of-terms precs))
  (define non-terms (map (λ (non-term) (make-non-term non-term #f))
                         list-of-non-terms))
  (define term-table (make-hasheq))
  (define non-term-table (make-hasheq))
  
  (for ([t (in-list terms)])
       (hash-set! term-table (gram-sym-symbol t) t))

  (for ([nt (in-list non-terms)])
       (hash-set! non-term-table (gram-sym-symbol nt) nt))

  ;; parse-prod: syntax-object -> gram-sym vector
  (define (parse-prod prod-so)
    (syntax-case prod-so ()
      [(PROD-RHS-SYM ...)
       (andmap identifier? (syntax->list prod-so))
       (begin
         (for ([t (in-list (syntax->list prod-so))]
               #:when (memq (syntax->datum t) end-terms))
              (raise-syntax-error
               'parser-production-rhs
               (format "~a is an end token and cannot be used in a production" (syntax->datum t))
               t))
         (for/vector ([s (in-list (syntax->list prod-so))])
                     (cond
                       [(hash-ref term-table (syntax->datum s) #f)]
                       [(hash-ref non-term-table (syntax->datum s) #f)]
                       [else (raise-syntax-error
                              'parser-production-rhs
                              (format "~a is not declared as a terminal or non-terminal" (syntax->datum s))
                              s)])))]
      [_ (raise-syntax-error
          'parser-production-rhs
          "production right-hand-side must have form (symbol ...)"
          prod-so)]))

  ;; parse-action: syntax-object * syntax-object -> syntax-object
  (define (parse-action rhs act-in)
    (define-values (args biggest) (get-args 1 (syntax->list rhs) src-pos term-defs))
    (define act 
      (if biggest
          (with-syntax ([(CAR-BIGGEST . CDR-BIGGEST) biggest]
                        [$N-START-POS (datum->syntax (car biggest) '$n-start-pos)]
                        [$N-END-POS (datum->syntax (cdr biggest) '$n-end-pos)]
                        [ACT-IN act-in])
            #'(let ([$N-START-POS CAR-BIGGEST]
                    [$N-END-POS CDR-BIGGEST])
                ACT-IN))
          act-in))
    (with-syntax ([ARGS args][ACT act])
      (syntax/loc #'ACT (λ ARGS ACT))))

  ;; parse-prod+action: non-term * syntax-object -> production
  (define (parse-prod+action nt prod-so)
    (syntax-case prod-so ()
      [(PROD-RHS ACTION)
       (let ([p (parse-prod #'PROD-RHS)])
         (make-prod 
          nt
          p
          #f
          (let loop ([i (sub1 (vector-length p))])
            (if (>= i 0)
                (let ([gs (vector-ref p i)])
                  (if (term? gs)
                      (term-prec gs)
                      (loop (sub1 i))))
                #f))
          (parse-action #'PROD-RHS #'ACTION)))]
      [(PROD-RHS (PREC TERM) ACTION)
       (identifier? #'TERM)
       (let ([p (parse-prod #'PROD-RHS)])
         (make-prod 
          nt 
          p
          #f
          (term-prec
           (cond
             [(hash-ref term-table (syntax->datum #'TERM) #f)]
             [else (raise-syntax-error
                    'parser-production-rhs
                    (format
                     "unrecognized terminal ~a in precedence declaration"
                     (syntax->datum #'TERM))
                    #'TERM)]))
          (parse-action #'PROD-RHS #'ACTION)))]
      [_ (raise-syntax-error
          'parser-production-rhs
          "production must have form [(symbol ...) expression] or [(symbol ...) (prec symbol) expression]"
          prod-so)]))

  ;; parse-prod-for-nt: syntax-object -> production list
  (define (parse-prods-for-nt prods-so)
    (syntax-case prods-so ()
      [(NT PRODUCTIONS ...)
       (positive? (length (syntax->list #'(PRODUCTIONS ...))))
       (let ([nt (hash-ref non-term-table (syntax->datum #'NT))])
         (map (λ (p) (parse-prod+action nt p)) (syntax->list #'(PRODUCTIONS ...))))]
      [_ (raise-syntax-error
          'parser-productions
          "A production for a non-terminal must be (non-term right-hand-side ...) with at least 1 right hand side"
          prods-so)]))
      
  (for ([sstx (in-list start)]
        [ssym (in-list start-syms)]
        #:unless (memq ssym list-of-non-terms))
       (raise-syntax-error
        'parser-start
        (format "Start symbol ~a not defined as a non-terminal" ssym)
        sstx))

  (define starts (map (λ (x) (make-non-term (gensym) #f)) start-syms))
  (define end-non-terms (map (λ (x) (make-non-term (gensym) #f)) start-syms))
  (define parsed-prods (map parse-prods-for-nt (syntax->list prods)))
  (define start-prods (for/list ([start (in-list starts)]
                                 [end-non-term (in-list end-non-terms)])
                                (list (make-prod start (vector end-non-term) #f #f #'values))))
  (define new-prods 
    (append start-prods
            (for/list ([end-nt (in-list end-non-terms)]
                       [start-sym (in-list start-syms)])
                      (for/list ([end (in-list end-terms)])
                                (make-prod end-nt
                                           (vector
                                            (hash-ref non-term-table start-sym)
                                            (hash-ref term-table end))
                                           #f
                                           #f
                                           #'values)))
            parsed-prods))
          
  (make-object grammar%
    new-prods
    (map car start-prods)
    terms
    (append starts (append end-non-terms non-terms))
    (map (λ (term-name) (hash-ref term-table term-name)) end-terms)))
