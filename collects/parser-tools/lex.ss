(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See doc.txt.
	
  (require-for-syntax "private-lex/generate-code.ss")
  (require-for-syntax "private-lex/structs.ss")
  (require (lib "list.ss")
           (lib "readerr.ss" "syntax")
           "private-lex/token.ss")
  (provide lexer lexer-src-loc define-lex-abbrev define-lex-abbrevs
	   make-lex-buf
	   get-position position-offset position-line position-col position?
           define-tokens define-empty-tokens)
  
  
  (define-syntaxes (lexer lexer-src-loc)
    (let ((code
           (lambda (wrap)
             `(letrec ((match
                        (lambda (lb first-pos longest-match-length longest-match-action length)
                          (let ((match
                                 (push-back lb (- length longest-match-length)))
                                (end-pos (get-position lb)))
                            (if (not longest-match-action)
                                (raise-read-error
                                 (format "lexer: No match found in input starting with: ~a"
                                         (list->string (filter char? (lex-buffer-from lb))))
                                 #f
                                 (position-line first-pos)
                                 (position-col first-pos)
                                 (position-offset first-pos)
                                 (- (position-offset end-pos) (position-offset first-pos))))
                            (,wrap
                             (longest-match-action
                              (lambda ()
                                first-pos)
                              (lambda ()
                                end-pos)
                              (lambda ()
                                (if (char? (car match))
                                    (list->string (reverse match))
                                    (list->string (reverse (cdr match)))))
                              lb))))))
                (lambda (lb)
                  (unless (lex-buffer? lb)
                    (raise-type-error 
                     'lexer 
                     "lex-buf"
                     0
                     lb))
                  (let ((first-pos (get-position lb)))
                    (let lexer-loop (
                                     ;; current-state
                                     (state start-state)
                                     ;; the character to transition on
                                     (char (next-char lb))
                                     ;; action for the longest match seen thus far
                                     ;; including a match at the current state
                                     (longest-match-action 
                                      (vector-ref actions start-state))
                                     ;; how many characters have been read
                                     ;; including the one just read
                                     (length 1)
                                     ;; how many characters are in the longest match
                                     (longest-match-length 0))
                      (let ((next-state
                             (cond
                               ((eof-object? char)
                                (vector-ref eof-table state))
                               (else
                                (vector-ref 
                                 trans-table
                                 (bitwise-ior (char->integer char)
                                              (arithmetic-shift state 8)))))))
                        (cond
                          ((not next-state) (match lb
                                              first-pos
                                              longest-match-length
                                              longest-match-action
                                              length))
                          (else
                           (let ((act (vector-ref actions next-state)))
                             (lexer-loop next-state 
                                         (next-char lb)
                                         (if act
                                             act
                                             longest-match-action)
                                         (add1 length)
                                         (if act
                                             length
                                             longest-match-length)))))))))))))
      (values
       (lambda (stx)
         (syntax-case stx ()
           ((_)
            (raise-syntax-error #f "empty lexer is not allowed" stx))
           ((_ re-act ...)
            (begin
              (for-each
               (lambda (x)
                 (syntax-case x ()
                   ((re act) (void))
                   (_ (raise-syntax-error 'lexer 
                                          "expects regular expression / action pairs"
                                          x))))
               (syntax->list (syntax (re-act ...))))
              (let* ((table (generate-table (syntax (re-act ...)) stx))
                     (code
                      `(let ((start-state ,(table-start table))
                             (trans-table ,(table-trans table))
                             (eof-table ,(table-eof table))
                             (actions (vector ,@(vector->list (table-actions table)))))
                         ,(code `(lambda (x) x)))))
                (datum->syntax-object #'here code #f))))))
       (lambda (stx)
         (syntax-case stx ()
           ((_)
            (raise-syntax-error #f "empty lexer is not allowed" stx))
           ((_ re-act ...)
            (begin
              (for-each
               (lambda (x)
                 (syntax-case x ()
                   ((re act) (void))
                   (_ (raise-syntax-error 'lexer 
                                          "expects regular expression / action pairs"
                                          x))))
               (syntax->list (syntax (re-act ...))))
              (let* ((table (generate-table (syntax (re-act ...)) stx))
                     (code
                      `(let ((start-state ,(table-start table))
                             (trans-table ,(table-trans table))
                             (eof-table ,(table-eof table))
                             (actions (vector ,@(vector->list (table-actions table)))))
                         ,(code `(lambda (x) (list x first-pos end-pos))))))
                (datum->syntax-object #'here code #f)))))))))
  
  
  (define-syntax (define-lex-abbrev stx)
    (syntax-case stx ()
      ((_ name re)
       (syntax
        (define-syntax name
          (make-lex-abbrev (quote-syntax re)))))
      (_ 
       (raise-syntax-error
        #f
        "Form should be (define-lex-abbrev name re)"
        stx))))

  (define-syntax (define-lex-abbrevs stx)
    (syntax-case stx ()
      ((_ x ...)
       (let* ((abbrev (syntax->list (syntax (x ...))))
              (r (map (lambda (a)
                        (syntax-case a ()
                          ((name re)
                           (identifier? (syntax name))
                           (syntax (define-lex-abbrev name re)))
                          (_ (raise-syntax-error
                              'Lexer-abbreviation 
                              "Form should be (identifier value)"
                              a))))
                      abbrev)))
         (datum->syntax-object
          #'here
          (cons 'begin r)
          stx)))
      (_
       (raise-syntax-error
        #f
        "Form should be (define-lex-abbrevs (name re) ...)"
        stx))))

  ;; Lex buffer is NOT thread safe


  ;; c = char | eof
  ;; lex-buf = 
  ;;   (make-lex-buffer input-port (c list) (c list) int int int (int list))
  (define-struct lex-buffer (ip from to offset line col line-lengths tab-skips))

  ;; make-lex-buf: input-port -> lex-buf
  (define make-lex-buf
    (case-lambda 
     ((ip) 
      (cond
       ((not (input-port? ip))
	(raise-type-error 'make-lex-buf "input-port" 0 ip))
       (else
	(make-lex-buffer ip null null 1 1 1 null null))))
     ((ip offsets)
      (cond
       ((not (input-port? ip))
	(raise-type-error 'make-lex-buf "input-port" 0 ip offsets))
       ((or (not (= 3 (length offsets)))
	    (not (andmap integer? offsets))
	    (not (andmap exact? offsets))
	    (not (andmap (lambda (x) (>= x 0)) offsets)))
	(raise-type-error 'make-lex-buf "list of 3 non-negative exact integers" 1 ip offsets))
       (else
	(make-lex-buffer ip null null (add1 (caddr offsets)) (add1 (car offsets)) (add1 (cadr offsets)) null null))))))

  ;; next-char: lex-buf -> c
  ;; gets the next character from the buffer
  (define (next-char lb)
    (let ((get-next
           (lambda ()
	     (cond
	      ((null? (lex-buffer-from lb)) 
	       (read-char (lex-buffer-ip lb)))
	      (else 
	       (begin0
		(car (lex-buffer-from lb))
		(set-lex-buffer-from! lb (cdr (lex-buffer-from lb)))))))))
      (let ((char-in
	     (let ((real-char (get-next)))
	       (if (eq? #\return real-char)
		   (let ((second-char (get-next)))
		     (if (not (eq? second-char #\newline))
			 (set-lex-buffer-from! 
			  lb 
			  (cons second-char (lex-buffer-from lb))))
		     #\newline)
		   real-char))))
	(set-lex-buffer-to! lb (cons char-in (lex-buffer-to lb)))
	(cond 
	 ((eq? #\tab char-in)
	  (let ((skip-amt (- 8 (modulo (lex-buffer-col lb) 8))))
            (set-lex-buffer-tab-skips! lb (cons skip-amt (lex-buffer-tab-skips lb)))
	    (set-lex-buffer-col! lb (+ skip-amt (lex-buffer-col lb)))))
	 ((eq? #\newline char-in)
	  (set-lex-buffer-line-lengths!
	   lb
	   (cons (lex-buffer-col lb)
		 (lex-buffer-line-lengths lb)))
	  (set-lex-buffer-line! lb (add1 (lex-buffer-line lb)))
	  (set-lex-buffer-col! lb 1))
	 (else
	  (set-lex-buffer-col! lb (add1 (lex-buffer-col lb)))))
        (set-lex-buffer-offset! lb (add1 (lex-buffer-offset lb)))
        char-in)))
  
  ;; push-back: lex-buf * int -> c list
  ;; pushes the last read i characters back to be read again
  ;; returns the characters not pushed back and read after the last push-back
  ;; (in reverse order)
  (define (push-back lb i)
    (letrec (
	     ;; switch-buffers: c list * c list * int -> clist * clist
	     ;; moves num-to-add characters from from to to.
	     ;; (switch-buffers '(1 2 3 4) '(5 6 7 8) 2) =
	     ;;   (values '(3 4) '(2 1 5 6 7 8))
	     ;; (switch-buffers '(4 3 2 1) '(5 6 7 8) 2) =
	     ;;   (values '(2 1) '(3 4 5 6 7 8))
	     (switch-buffers
	      (lambda (from to num-to-add)
		(cond
		 ((= 0 num-to-add) (values from to))
		 (else
		  (cond
		   ((eq? #\newline (car from))
		    (set-lex-buffer-line! lb (sub1 (lex-buffer-line lb)))
		    (set-lex-buffer-col! lb (car (lex-buffer-line-lengths lb)))
		    (set-lex-buffer-line-lengths! lb (cdr (lex-buffer-line-lengths lb))))
                   ((eq? #\tab (car from))
                    (set-lex-buffer-col! lb (- (lex-buffer-col lb) 
                                               (car (lex-buffer-tab-skips lb))))
                    (set-lex-buffer-tab-skips! lb (cdr (lex-buffer-tab-skips lb))))
                   (else
                    (set-lex-buffer-col! lb (sub1 (lex-buffer-col lb)))))
                  (switch-buffers (cdr from) 
                                  (cons (car from) to) 
                                  (sub1 num-to-add)))))))
      (let-values (((ret new-from)
		    (switch-buffers (lex-buffer-to lb)
				    (lex-buffer-from lb)
				    i)))
        (set-lex-buffer-from! lb new-from)
        (set-lex-buffer-to! lb null)
        (set-lex-buffer-offset! lb (- (lex-buffer-offset lb) i))
        (set-lex-buffer-line-lengths! lb null)
        (set-lex-buffer-tab-skips! lb null)
        ret)))

  (define-struct position (offset line col))
  (define (get-position lb)
    (make-position (lex-buffer-offset lb)
		   (lex-buffer-line lb)
		   (lex-buffer-col lb)))


)


