#lang racket/base
(require "util.rkt" syntax/id-table)
(provide parse)
  
(define (bad-args stx num)
  (raise-syntax-error #f (format "incorrect number of arguments (should have ~a)" num) stx))

;; char-range-arg: syntax-object syntax-object -> nat
;; If c contains is a character or length 1 string, returns the integer
;; for the character.  Otherwise raises a syntax error.
(define (char-range-arg stx containing-stx)
  (define c (syntax-e stx))
  (cond
    [(char? c) (char->integer c)]
    [(and (string? c) (= (string-length c) 1))
     (char->integer (string-ref c 0))]
    [else
     (raise-syntax-error
      #f
      "not a char or single-char string"
      containing-stx stx)]))

(module+ test
  (check-equal? (char-range-arg #'#\1 #'here) (char->integer #\1))
  (check-equal? (char-range-arg #'"1" #'here) (char->integer #\1)))

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))
(define (disarm stx)
  (syntax-disarm stx orig-insp))

;; parse : syntax-object (box (list-of syntax-object)) -> s-re (see re.rkt)
;; checks for errors and generates the plain s-exp form for s
;; Expands lex-abbrevs and applies lex-trans.
(define (parse stx disappeared-uses)
  (let loop ([stx stx]
             [disappeared-uses disappeared-uses]
             ;; seen-lex-abbrevs: id-table
             [seen-lex-abbrevs (make-immutable-free-id-table)])
    (let ([recur (λ (s)
                   (loop (syntax-rearm s stx)
                         disappeared-uses
                         seen-lex-abbrevs))]
          [recur/abbrev (λ (s id)
                          (loop (syntax-rearm s stx)
                                disappeared-uses
                                (free-id-table-set seen-lex-abbrevs id id)))])
      (syntax-case (disarm stx) (repetition union intersection complement concatenation
                                            char-range char-complement)
        [_
         (identifier? stx)
         (let ([expansion (syntax-local-value stx (λ () #f))])
           (unless (lex-abbrev? expansion)
             (raise-syntax-error 'regular-expression
                                 "undefined abbreviation"
                                 stx))
           ;; Check for cycles.
           (when (free-id-table-ref seen-lex-abbrevs stx (λ () #f))
             (raise-syntax-error 'regular-expression
                                 "illegal lex-abbrev cycle detected"
                                 stx
                                 #f
                                 (list (free-id-table-ref seen-lex-abbrevs stx))))
           (set-box! disappeared-uses (cons stx (unbox disappeared-uses)))
           (recur/abbrev ((lex-abbrev-get-abbrev expansion)) stx))]
        [_
         (or (char? (syntax-e stx)) (string? (syntax-e stx)))
         (syntax-e stx)]
        [(repetition ARG ...)
         (let ([arg-list (syntax->list #'(ARG ...))])
           (unless (= 3 (length arg-list))
             (bad-args stx 2))
           (define low (syntax-e (car arg-list)))
           (define high (syntax-e (cadr arg-list)))
           (define re (caddr arg-list))
           (unless (and (number? low) (exact? low) (integer? low) (>= low 0))
             (raise-syntax-error #f "not a non-negative exact integer" stx (car arg-list)))
           (unless (or (and (number? high) (exact? high) (integer? high) (>= high 0))
                       (eqv? high +inf.0))
             (raise-syntax-error #f "not a non-negative exact integer or +inf.0" stx (cadr arg-list)))
           (unless (<= low high)
             (raise-syntax-error #f "the first argument is not less than or equal to the second argument" stx))
           `(repetition ,low ,high ,(recur re)))]
        [(union RE ...)
         `(union ,@(map recur (syntax->list #'(RE ...))))]
        [(intersection RE ...)
         `(intersection ,@(map recur (syntax->list #'(RE ...))))]
        [(complement RE ...)
         (let ([re-list (syntax->list #'(RE ...))])
           (unless (= 1 (length re-list))
             (bad-args stx 1))
           `(complement ,(recur (car re-list))))]
        [(concatenation RE ...)
         `(concatenation ,@(map recur (syntax->list #'(RE ...))))]
        [(char-range ARG ...)
         (let ((arg-list (syntax->list #'(ARG ...))))
           (unless (= 2 (length arg-list))
             (bad-args stx 2))
           (let ([i1 (char-range-arg (car arg-list) stx)]
                 [i2 (char-range-arg (cadr arg-list) stx)])
             (if (<= i1 i2)
                 `(char-range ,(integer->char i1) ,(integer->char i2))
                 (raise-syntax-error #f "the first argument does not precede or equal second argument" stx))))]
        [(char-complement ARG ...)
         (let ([arg-list (syntax->list #'(ARG ...))])
           (unless (= 1 (length arg-list))
             (bad-args stx 1))
           (define parsed (recur (car arg-list)))
           (unless (char-set? parsed)
             (raise-syntax-error #f "not a character set" stx (car arg-list)))
           `(char-complement ,parsed))]
        ((OP form ...)
         (identifier? #'OP)
         (let* ([expansion (syntax-local-value #'OP (λ () #f))])
           (set-box! disappeared-uses (cons #'OP (unbox disappeared-uses)))
           (cond
             [(lex-trans? expansion)
              (recur ((lex-trans-f expansion) (disarm stx)))]
             [expansion
              (raise-syntax-error 'regular-expression "not a lex-trans" stx)]
             [else
              (raise-syntax-error 'regular-expression "undefined operator" stx)])))
        [_ (raise-syntax-error 'regular-expression "not a char, string, identifier, or (op args ...)" stx)]))))
       

  
;; char-set? : s-re -> bool
;; A char-set is an re that matches only strings of length 1.
;; char-set? is conservative.
(define (char-set? s-re)
  (cond
    [(char? s-re)]
    [(string? s-re) (= (string-length s-re) 1)]
    [(list? s-re) (case (car s-re)
                    [(union intersection) (andmap char-set? (cdr s-re))]
                    [(char-range char-complement) #t]
                    [(repetition) (and (= (cadr s-re) (caddr s-re)) (char-set? (cadddr s-re)))]
                    [(concatenation) (and (= 2 (length s-re)) (char-set? (cadr s-re)))]
                    (else #f))]
    [else #f]))
  
(module+ test
  (require rackunit)
  (check-equal? (char-set? #\a) #t)
  (check-equal? (char-set? "12") #f)
  (check-equal? (char-set? "1") #t)
  (check-equal? (char-set? '(repetition 1 2 #\1)) #f)
  (check-equal? (char-set? '(repetition 1 1 "12")) #f)
  (check-equal? (char-set? '(repetition 1 1 "1")) #t)
  (check-equal? (char-set? '(union "1" "2" "3")) #t)
  (check-equal? (char-set? '(union "1" "" "3")) #f)
  (check-equal? (char-set? '(intersection "1" "2" (union "3" "4"))) #t)
  (check-equal? (char-set? '(intersection "1" "")) #f)
  (check-equal? (char-set? '(complement "1")) #f)
  (check-equal? (char-set? '(concatenation "1" "2")) #f)
  (check-equal? (char-set? '(concatenation "" "2")) #f)
  (check-equal? (char-set? '(concatenation "1")) #t)
  (check-equal? (char-set? '(concatenation "12")) #f)
  (check-equal? (char-set? '(char-range #\1 #\2)) #t)
  (check-equal? (char-set? '(char-complement #\1)) #t))

;; yikes... these test cases all have the wrong arity, now.
;; and by "now", I mean it's been broken since before we 
;; moved to git. 
(module+ test
  (check-equal? (parse #'#\a null) #\a)
  (check-equal? (parse #'"1" null) "1")
  (check-equal? (parse #'(repetition 1 1 #\1) null)
                '(repetition 1 1 #\1))
  (check-equal? (parse #'(repetition 0 +inf.0 #\1) null) '(repetition 0 +inf.0 #\1))
  (check-equal? (parse #'(union #\1 (union "2") (union)) null)
                '(union #\1 (union "2") (union)))
  (check-equal? (parse #'(intersection #\1 (intersection "2") (intersection))
                       null)
                '(intersection #\1 (intersection "2") (intersection)))
  (check-equal? (parse #'(complement (union #\1 #\2))
                       null)
                '(complement (union #\1 #\2)))
  (check-equal? (parse #'(concatenation "1" "2" (concatenation)) null)
                '(concatenation "1" "2" (concatenation)))
  (check-equal? (parse #'(char-range "1" #\1) null) '(char-range #\1 #\1))
  (check-equal? (parse #'(char-range #\1 "1") null) '(char-range #\1 #\1))
  (check-equal? (parse #'(char-range "1" "3") null) '(char-range #\1 #\3))
  (check-equal? (parse #'(char-complement (union "1" "2")) null)
                '(char-complement (union "1" "2"))))
