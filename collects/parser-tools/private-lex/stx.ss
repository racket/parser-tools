(module stx mzscheme
  (require "util.ss")
  
  (provide parse)
         
  (define (repetition-error stx)
    (raise-syntax-error
     'regular-expression
     "must be (repetition non-negative-exact-integer non-negative-exact-integer-or-+inf.0 re)"
     stx))
  
  (define (char-range-error stx)
    (raise-syntax-error
     'regular-expression
     "must be (char-range char-or-single-char-string char-or-single-char-string)"
     stx))
  
  (define (char-range-arg c stx)
    (cond
      ((char? c) (integer->char c))
      ((and (string? c (= string-length c 1)))
       (integer->char (string-ref c 0)))
      (else
       (char-range-error stx))))
  
  ;; parse : syntax-object -> s-re (see re.ss)
  ;; checks for errors and generates the plain s-exp form for s
  (define (parse stx)
    (syntax-case stx (repetition union intersection complement concatenation
                      char-range char-complement)
      (_
       (identifier? stx)
       (let ((expansion (syntax-local-value stx (lambda () #f))))
         (unless (lex-abbrev? expansion)
           (raise-syntax-error 'regular-expression
                               "undefined abbreviation"
                               stx))
         (parse (lex-abbrev-abbrev expand))))
      (_
       (or (char? (syntax-e stx)) (string? (syntax-e stx)))
       (syntax-e stx))
      ((repetition arg ...)
       (let ((arg-list (syntax->list (syntax (arg ...)))))
         (unless (= 3 (length arg-list))
           (repetition-error stx))
         (let ((lo-val (car arg-list))
               (hi-val (cadr arg-list))
               (re (caddr arg-list)))
           (unless (and (exact? lo-val) (integer? lo-val) (> lo-val 0)
                        (or (and (exact? hi-val) (integer? hi-val) (> hi-val 0))
                            (eq? hi-val +inf.0)))
             (repetition-error stx))
           `(repetition ,lo-val ,hi-val ,(parse re)))))
      ((union re ...)
       `(union ,@(map parse (syntax->list (syntax (re ...))))))
      ((intersection re ...)
       `(intersection ,@(map parse (syntax->list (syntax (re ...))))))
      ((complement re ...)
       (let ((re-list (syntax->list (syntax (re ...)))))
         (unless (= 1 (length re-list))
           (raise-syntax-error 'regular-expression
                               "must be (complement re)"
                               stx))
         `(complement ,(parse (car re-list)))))
      ((concatenation re ...)
       `(concatenation ,@(map parse (syntax->list (syntax (re ...))))))
      ((char-range arg ...)
       (let ((arg-list (syntax->list (syntax (arg ...)))))
         (unless (= 2 (length arg-list))
           (char-range-error stx))
         (let ((i1 (char-range-arg (car arg-list) stx))
               (i2 (char-range-arg (cadr arg-list) stx)))
           (if (<= i1 i2)
               `(char-range ,(integer->char i1) ,(integer->char i2))
               (raise-syntax-error
                'regular-expression
                (format "first argument ~a does not preceed second argument ~a" 
                        (car arg-list) (cdr arg-list))
                stx)))))
      ((char-complement arg ...)
       (let ((arg-list (syntax->list (syntax (arg ...)))))
         (unless (= 1 (length arg-list))
           (raise-syntax-error
            'regular-expression
            "must be (char-complement char-set-re)"
            stx))
         (let ((parsed (parse (car arg-list))))
           (unless (pure-char? parsed)
             (raise-syntax-error
              'regular-expression
              "must be (char-complement char-set-re)"
              stx))
           `(char-complement ,parsed))))
       ((op form ...)
        (identifier? (syntax op))
        (let ((expansion (syntax-local-value (syntax op) (lambda () #f))))
          (unless (lex-trans? expansion)
            (raise-syntax-error 'regular-expression
                                "undefined operator in"
                                stx))
          (parse ((lex-trans-f expansion) stx))))
      (_ 
       (raise-syntax-error
        'regular-expression
        "must be char, string, identifier, or (op args ...)"
        stx))))
       
      
  
  (define (pure-char? s-re)
    (cond
      ((char? s-re) #t)
      ((string? s-re) (= (string-length s-re) 1))
      ((list? s-re) 
       (let ((op (car s-re)))
         (case op
           ((union intersection) (andmap pure-char? (cdr s-re)))
           ((char-range char-complement) #t)
           (else #f))))
      (else #f)))
           
           
  )