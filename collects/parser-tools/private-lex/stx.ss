(module stx mzscheme
  (require (lib "stx.ss" "syntax")
           "util.ss")
  
  (provide parse)
  
  
  (define (num-arg-err s expect given)
    (raise-syntax-error
     'regular-expression
     (format "operator expects ~a arguments, given ~a" expect given)
     s))

  
  ;; parse : syntax-object -> s-re (see re.ss)
  ;; checks for errors and generates the plain s-exp form for s
  (define (parse s)
    (let ((s-e (syntax-e s)))
      (cond
        ((char? s-e) s-e)
        ((string? s-e) s-e)
        ((symbol? s-e)
         (let ((expand (syntax-local-value s (lambda () #f))))
           (unless (lex-abbrev? expand)
             (raise-syntax-error 'regular-expression "undefined abbreviation" s))
           (parse (lex-abbrev-abbrev expand))))
        ((stx-null? s)
         (raise-syntax-error 'regular-expression "invalid regular expression" s))
        ((stx-list? s)
         (let* ((ar (stx->list (stx-cdr s)))
                (num-args (length ar)))
           (case (syntax-e (stx-car s))
             ((epsilon) '(epsilon))
             ((*)
              (unless (= num-args 1)
                (num-arg-err s 1 num-args))
              `(* ,(parse (car ar))))
             ((+)
              (unless (= num-args 1)
                (num-arg-err s 1 num-args))
              `(+ ,(parse (car ar))))
             ((?)
              (unless (= num-args 1)
                (num-arg-err s 1 num-args))
              `(? ,(parse (car ar))))
             ((~)
              (unless (= num-args 1)
                (num-arg-err s 1 num-args))
              `(~ ,(parse (car ar))))
             ((:) `(: ,@(map parse ar)))
             ((&) `(& ,@(map parse ar)))
             ((@) `(@ ,@(map parse ar)))
             ((-)
              (unless (= num-args 2)
                (num-arg-err s 2 num-args))
              (let ((c1 (parse (car ar)))
                    (c2 (parse (cadr ar))))
                (if (and (or (char? c1) (and (string? c1) (= 1 (string-length c1))))
                         (or (char? c2) (and (string? c2) (= 1 (string-length c2)))))
                    (let ((i1 (char->integer (if (char? c1) c1 (string-ref c1 0))))
                          (i2 (char->integer (if (char? c2) c2 (string-ref c2 0)))))
                      (if (<= i1 i2)
                          `(- ,c1 ,c2)
                          (raise-syntax-error
                           'regular-expression
                           (format "first argument ~a does not preceed second argument ~a" 
                                   c1 c2)
                           s)))
                    (raise-syntax-error
                     'regular-expression
                     (format "expects single character arguments, given ~a and ~a"
                             (syntax-object->datum (car ar))
                             (syntax-object->datum (cadr ar)))
                     s))))
             ((^)
              (let ((res (map parse ar)))
                (if (not (andmap pure-char? res))
                    (raise-syntax-error
                     'regular-expression
                     (format 
                      "expects single character or character range arguments, given ~a"
                      (map syntax-object->datum ar))
                     s))
                `(^ ,@res)))
             (else
              (raise-syntax-error
               'regular-expression
               "invalid operator"
               s)))))
        (else
         (raise-syntax-error
          'regular-expression
          "invalid regular expression"
          s)))))

  (define (pure-char? s-re)
    (cond
      ((char? s-re) #t)
      ((string? s-re) (= (string-length s-re) 1))
      ((list? s-re) 
       (let ((op (car s-re)))
         (case op
           ((: ^) (andmap pure-char? (cdr s-re)))
           ((-) #t)
           (else #f))))
      (else #f)))
           
           
  )