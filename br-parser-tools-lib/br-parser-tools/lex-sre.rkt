#lang racket/base
(require (for-syntax racket/base)
         br-parser-tools/lex)
    
(provide (rename-out [sre-* *]
                     [sre-+ +]
                     [sre-= =]
                     [sre->= >=]
                     [sre-or or]
                     [sre-- -]
                     [sre-/ /])
         ? ** : seq & ~  /-only-chars)
           
(define-lex-trans (sre-* stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(repetition 0 +inf.0 (union RE ...))]))

(define-lex-trans (sre-+ stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(repetition 1 +inf.0 (union RE ...))]))

(define-lex-trans (? stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(repetition 0 1 (union RE ...))]))
  
(define-lex-trans (sre-= stx)
  (syntax-case stx ()
    [(_ N RE ...)
     #'(repetition N N (union RE ...))]))
  
(define-lex-trans (sre->= stx)
  (syntax-case stx ()
    [(_ N RE ...)
     #'(repetition N +inf.0 (union RE ...))]))

(define-lex-trans (** stx)
  (syntax-case stx ()
    [(_ LOW #f RE ...)
     #'(** LOW +inf.0 RE ...)]
    [(_ LOW HIGH RE ...)
     #'(repetition LOW HIGH (union RE ...))]))
  
(define-lex-trans (sre-or stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(union RE ...)]))
  
(define-lex-trans (: stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(concatenation RE ...)]))

(define-lex-trans (seq stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(concatenation RE ...)]))

(define-lex-trans (& stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(intersection RE ...)]))

(define-lex-trans (~ stx)
  (syntax-case stx ()
    [(_ RE ...)
     #'(char-complement (union RE ...))]))
  
;; set difference
(define-lex-trans (sre-- stx)
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f
                         "must have at least one argument"
                         stx)]
    [(_ BIG-RE RE ...)
     #'(& BIG-RE (complement (union RE ...)))]))
  
(define-lex-trans (sre-/ stx)
  (syntax-case stx ()
    [(_ RANGE ...)
     (let ([chars
            (apply append (for/list ([r (in-list (syntax->list #'(RANGE ...)))])
                                 (let ([x (syntax-e r)])
                                   (cond
                                     [(char? x) (list x)]
                                     [(string? x) (string->list x)]
                                     [else
                                      (raise-syntax-error #f "not a char or string" stx r)]))))])
       (unless (even? (length chars))
         (raise-syntax-error #f "not given an even number of characters" stx))
       #`(/-only-chars #,@chars))]))
  
(define-lex-trans (/-only-chars stx)
  (syntax-case stx ()
    [(_ C1 C2)
     #'(char-range C1 C2)]
    [(_ C1 C2 C ...)
     #'(union (char-range C1 C2) (/-only-chars C ...))]))
  
   
