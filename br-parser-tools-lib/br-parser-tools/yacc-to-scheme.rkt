#lang racket/base
(require br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         br-parser-tools/yacc
         syntax/readerr
         racket/list)
(provide trans)
  
(define match-double-string
  (lexer
   [(:+ (:~ #\" #\\)) (append (string->list lexeme)
                              (match-double-string input-port))]
   [(:: #\\ any-char) (cons (string-ref lexeme 1) (match-double-string input-port))]
   [#\" null]))

(define match-single-string
  (lexer
   [(:+ (:~ #\' #\\)) (append (string->list lexeme)
                              (match-single-string input-port))]
   [(:: #\\ any-char) (cons (string-ref lexeme 1) (match-single-string input-port))]
   [#\' null]))
  
(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ "A" "Z"))]
  [digit (:/ "0" "9")]
  [initial (:or letter (char-set "!$%&*/<=>?^_~@"))]
  [subsequent (:or initial digit (char-set "+-.@"))]
  [comment (:: "/*" (complement (:: any-string "*/" any-string)) "*/")])

(define-empty-tokens x (EOF PIPE |:| SEMI |%%| %prec))
(define-tokens y (SYM STRING))
  
(define get-token-grammar
  (lexer-src-pos
   ["%%" '|%%|]
   [":" (string->symbol lexeme)]
   ["%prec" (string->symbol lexeme)]
   [#\| 'PIPE]
   [(:+ (:or #\newline #\tab " " comment (:: "{" (:* (:~ "}")) "}")))
    (return-without-pos (get-token-grammar input-port))]
   [#\; 'SEMI]
   [#\' (token-STRING (string->symbol (list->string (match-single-string input-port))))]
   [#\" (token-STRING (string->symbol (list->string (match-double-string input-port))))]
   [(:: initial (:* subsequent)) (token-SYM (string->symbol lexeme))]))

(define (parse-grammar enter-term enter-empty-term enter-non-term)
  (parser
   (tokens x y)
   (src-pos)
   (error (λ (tok-ok tok-name tok-value start-pos end-pos)
            (raise-read-error
             (format "Error Parsing YACC grammar at token: ~a with value: ~a" tok-name tok-value)
             (file-path)
             (position-line start-pos)
             (position-col start-pos)
             (position-offset start-pos)
             (- (position-offset end-pos) (position-offset start-pos)))))
	       
   (end |%%|)
   (start gram)
   (grammar
    (gram 
     ((production) (list $1))
     ((production gram) (cons $1 $2)))
    (production
     ((SYM |:| prods SEMI) 
      (begin
        (enter-non-term $1)
        (cons $1 $3))))
    (prods
     ((rhs) (list `(,$1 #f)))
     ((rhs prec) (list `(,$1 ,$2 #f)))
     ((rhs PIPE prods) (cons `(,$1 #f) $3))
     ((rhs prec PIPE prods) (cons `(,$1 ,$2 #f) $4)))
    (prec
     ((%prec SYM)
      (begin
        (enter-term $2)
        (list 'prec $2)))
     ((%prec STRING)
      (begin
        (enter-empty-term $2)
        (list 'prec $2))))
    (rhs
     (() null)
     ((SYM rhs) 
      (begin
        (enter-term $1)
        (cons $1 $2)))
     ((STRING rhs) 
      (begin
        (enter-empty-term $1)
        (cons $1 $2)))))))
  
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))
  
(define (trans filename)
  (define i (open-input-file filename))
  (define terms (make-hasheq))
  (define eterms (make-hasheq))
  (define nterms (make-hasheq))
  (define (enter-term s)
    (when (not (hash-ref nterms s (λ () #f)))
      (hash-set! terms s #t)))
  (define (enter-empty-term s)
    (when (not (hash-ref nterms s (λ () #f)))
      (hash-set! eterms s #t)))
  (define (enter-non-term s)
    (hash-remove! terms s)
    (hash-remove! eterms s)
    (hash-set! nterms s #t))
  (port-count-lines! i)
  (file-path filename)
  (regexp-match "%%" i)
  (begin0
    (let ([gram ((parse-grammar enter-term enter-empty-term enter-non-term)
                 (λ () 
                   (let ((t (get-token-grammar i)))
                     t)))])
      `(begin
         (define-tokens t ,(sort (hash-map terms (λ (k v) k)) symbol<?))
         (define-empty-tokens et ,(sort (hash-map eterms (λ (k v) k)) symbol<?))
         (parser
          (start ___)
          (end ___)
          (error ___)
          (tokens t et)
          (grammar ,@gram))))
    (close-input-port i)))
