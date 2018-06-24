#lang racket/base
(require "input-file-parser.rkt"
         "grammar.rkt"
         "table.rkt"
         racket/class
         racket/contract)
(require (for-template racket/base))
  
(provide/contract [build-parser (-> string? any/c any/c
                                    (listof identifier?)
                                    (listof identifier?)
                                    (listof identifier?)
                                    (or/c syntax? #f)
                                    syntax?
                                    (values any/c any/c any/c any/c))])
  
;; fix-check-syntax : (listof identifier?) (listof identifier?) (listof identifier?)
;;                    (union syntax? false/c) syntax?) -> syntax?
(define (fix-check-syntax input-terms start ends assocs prods)
  (define term-binders (get-term-list input-terms))
  (define get-term-binder
    (let ([t (make-hasheq)])
      (for ([term (in-list term-binders)])
           (hash-set! t (syntax-e term) term))
      (λ (x)
        (define r (hash-ref t (syntax-e x) (λ () #f)))
        (if r
            (syntax-local-introduce (datum->syntax r (syntax-e x) x x))
            x))))
  (define rhs-list (syntax-case prods ()
                     [((_ RHS ...) ...) (syntax->list #'(RHS ... ...))]))
  (with-syntax ([(TMP ...) (map syntax-local-introduce term-binders)]
                [(TERM-GROUP ...)
                 (map (λ (tg)
                        (syntax-property
                         (datum->syntax tg #f)
                         'disappeared-use
                         tg))
                      input-terms)]
                [(END ...) (map get-term-binder ends)]
                [(START ...) (map get-term-binder start)]
                [(BIND ...) (syntax-case prods ()
                              (((BIND _ ...) ...)
                               (syntax->list #'(BIND ...))))]
                [((BOUND ...) ...)
                 (map (λ (rhs)
                        (syntax-case rhs ()
                          [((BOUND ...) (_ PBOUND) __)
                           (map get-term-binder
                                (cons #'PBOUND (syntax->list #'(BOUND ...))))]
                          [((BOUND ...) _)
                           (map get-term-binder
                                (syntax->list #'(BOUND ...)))]))
                      rhs-list)]
                [(PREC ...)
                 (if assocs
                     (map get-term-binder
                          (syntax-case assocs ()
                            (((__ TERM ...) ...)
                             (syntax->list #'(TERM ... ...)))))
                     null)])
    #`(when #f
        (let ((BIND void) ... (TMP void) ...)
          (void BOUND ... ... TERM-GROUP ... START ... END ... PREC ...)))))

(require racket/list "parser-actions.rkt")

(define (build-parser filename src-pos suppress input-terms start end assocs prods)
  (define grammar (parse-input input-terms start end assocs prods src-pos))
  (define table (build-table grammar filename suppress))
  (define all-tokens (make-hasheq))
  (define actions-code `(vector ,@(map prod-action (send grammar get-prods))))

  (for ([term (in-list (send grammar get-terms))])
       (hash-set! all-tokens (gram-sym-symbol term) #t))
  
  #;(let ((num-states (vector-length table))
          (num-gram-syms (+ (send grammar get-num-terms)
                            (send grammar get-num-non-terms)))
          (num-ht-entries (apply + (map length (vector->list table))))
          (num-reduces
           (let ((ht (make-hasheq)))
             (for-each
              (λ (x)
                (when (reduce? x)
                  (hash-set! ht x #t)))
              (map cdr (apply append (vector->list table))))
             (length (hash-table-map ht void)))))
      (printf "~a states, ~a grammar symbols, ~a hash-table entries, ~a reduces\n"
              num-states num-gram-syms num-ht-entries num-reduces)
      (printf "~a -- ~aKB, previously ~aKB\n"
              (/ (+ 2 num-states
                    (* 4 num-states) (* 2 1.5 num-ht-entries)
                    (* 5 num-reduces)) 256.0)
              (/ (+ 2 num-states
                    (* 4 num-states) (* 2 2.3 num-ht-entries)
                    (* 5 num-reduces)) 256.0)
              (/ (+ 2 (* num-states num-gram-syms) (* 5 num-reduces)) 256.0)))
  (values table
          all-tokens
          actions-code
          (fix-check-syntax input-terms start end assocs prods)))
     
