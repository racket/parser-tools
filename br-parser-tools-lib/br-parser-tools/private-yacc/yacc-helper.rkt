#lang racket/base
(require (prefix-in rl: racket/list)
         "../private-lex/token-syntax.rkt")

;; General helper routines
(provide duplicate-list? remove-duplicates overlap? vector-andmap display-yacc)
    
(define (vector-andmap pred vec)
  (for/and ([item (in-vector vec)])
    (pred vec)))

;; duplicate-list?: symbol list -> #f | symbol
;; returns a symbol that exists twice in l, or false if no such symbol 
;; exists
(define (duplicate-list? syms)
  (rl:check-duplicates syms eq?))

;; remove-duplicates: syntax-object list -> syntax-object list
;; removes the duplicates from the lists
(define (remove-duplicates syms)
  (rl:remove-duplicates syms equal? #:key syntax->datum))

;; overlap?: symbol list * symbol list -> #f | symbol
;; Returns an symbol in l1 intersect l2, or #f is no such symbol exists
(define (overlap? syms1 syms2)
  (for/first ([sym1 (in-list syms1)]
              #:when (memq sym1 syms2))
    sym1))

  
(define (display-yacc grammar tokens start precs port)
  (let-syntax ([p (syntax-rules ()
                    ((_ args ...) (fprintf port args ...)))])
    (let* ([tokens (map syntax-local-value tokens)]
           [eterms (filter e-terminals-def? tokens)]
           [terms (filter terminals-def? tokens)]
           [term-table (make-hasheq)]
           [display-rhs
            (λ (rhs)
              (for ([sym (in-list (car rhs))])
                (p "~a " (hash-ref term-table sym (λ () sym))))
              (when (= 3 (length rhs))
                (p "%prec ~a" (cadadr rhs)))
              (p "\n"))])
      (for* ([t (in-list eterms)]
             [t (in-list (syntax->datum (e-terminals-def-t t)))])
        (hash-set! term-table t (format "'~a'" t)))
      (for* ([t (in-list terms)]
             [t (in-list (syntax->datum (terminals-def-t t)))])
        (p "%token ~a\n" t)
        (hash-set! term-table t (format "~a" t)))
      (when precs
        (for ([prec (in-list precs)])
          (p "%~a " (car prec))
          (for ([tok (in-list (cdr prec))])
            (p " ~a" (hash-ref term-table tok)))
          (p "\n")))
      (p "%start ~a\n" start)
      (p "%%\n")
      (for ([prod (in-list grammar)])
        (define nt (car prod))
        (p "~a: " nt)
        (display-rhs (cadr prod))
        (for ([rhs (in-list (cddr prod))])
          (p "| ")
          (display-rhs rhs))
        (p ";\n"))
      (p "%%\n"))))

  
  
