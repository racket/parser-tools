(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See doc.txt.
	
  (require-for-syntax "private-lex/generate-code.ss"
                      "private-lex/structs.ss")

  (require (lib "readerr.ss" "syntax")
           "private-lex/token.ss"
	   (lib "cffi.ss" "compiler"))

  (provide lexer lexer-src-pos define-lex-abbrev define-lex-abbrevs
	   make-lex-buf
	   get-position position-offset position-line position-col position?
           define-tokens define-empty-tokens)
  
  (define-syntaxes (lexer lexer-src-pos)
    (let ((build-lexer
           (lambda (wrap?)
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
                    (let ((table (generate-table (syntax (re-act ...)) stx)))
                      (with-syntax ((code (compile-table table))
                                    (actions-stx `(vector ,@(vector->list (table-actions table))))
                                    (wrap? wrap?))
                        (syntax
                         (compiled-lexer-body code actions-stx wrap?)))))))))))
      (values
       (build-lexer #f)
       (build-lexer #t))))

  (define-syntaxes (lexer-old lexer-src-pos-old)
    (let ((build-lexer
	   (lambda (wrap?)
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
		    (let ((table (generate-table (syntax (re-act ...)) stx)))
		      (with-syntax ((start-state-stx (table-start table))
				    (trans-table-stx (table-trans table))
				    (eof-table-stx (table-eof table))
				    (no-lookahead-stx (table-no-lookahead table))
				    (actions-stx `(vector ,@(vector->list (table-actions table))))
				    (wrap? wrap?))
			(syntax 
			 (lexer-body start-state-stx 
				     trans-table-stx
				     eof-table-stx
				     actions-stx
				     no-lookahead-stx
				     wrap?)))))))))))
      (values
       (build-lexer #f)
       (build-lexer #t))))

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

  (define (compiled-lexer-body lexer actions wrap?)
    (lambda (lb)
      (unless (lex-buffer? lb)
        (raise-type-error 
         'lexer 
         "lex-buf"
         0
         lb))
      (let ((first-pos (get-position lb)))
        (let-values (((longest-match-length longest-match-action)
                      (lexer lb next-char)))
          (do-match lb first-pos longest-match-length (vector-ref actions longest-match-action) wrap?)))))
  
  (define (lexer-body start-state trans-table eof-table actions no-lookahead wrap?)
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
                         (longest-match-length 1))
          (let ((next-state 
                 (cond
		  ((eof-object? char)
		   (vector-ref eof-table state))
		  (else
		   (vector-ref 
		    trans-table
                    (+ (char->integer (string-ref char 0)) (* 256 state)))))))
            (cond
              ((not next-state)
               (do-match lb first-pos longest-match-length longest-match-action wrap?))
              ((vector-ref no-lookahead next-state)
               (let ((act (vector-ref actions next-state)))
                 (do-match lb 
                           first-pos 
                           (if act length longest-match-length)
                           (if act act longest-match-action)
                           wrap?)))
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
                                 longest-match-length))))))))))
  
  (define (do-match lb first-pos longest-match-length longest-match-action wrap?)
    (let* ((match (get-match lb longest-match-length))
           (end-pos (get-position lb)))
      (if (not longest-match-action)
          (raise-read-error
           (format "lexer: No match found in input starting with: ~a" match)
           #f
           (position-line first-pos)
           (position-col first-pos)
           (position-offset first-pos)
           (- (position-offset end-pos) (position-offset first-pos))))
      (cond
        (wrap?
         (let/ec ret
           (list (longest-match-action
                  (lambda () first-pos)
                  (lambda () end-pos)
                  (lambda () match)
                  ret
                  lb) 
                 first-pos 
                 end-pos)))
        (else 
         (longest-match-action
          (lambda () first-pos)
          (lambda () end-pos)
          (lambda () match)
          (lambda (x) x)
          lb)))))
  
  
  ;; Lex buffer is NOT thread safe
  ;; c = char | eof
  ;; lex-buf = 
  ;;   (make-lex-buffer input-port int int int int)
  (define-struct lex-buffer (ip peek-amt line-start col-start offset-start))

  ;; make-lex-buf: input-port -> lex-buf
  (define make-lex-buf
    (case-lambda 
     ((ip) 
      (cond
       ((not (input-port? ip))
	(raise-type-error 'make-lex-buf "input-port" 0 ip))
       (else
        (port-count-lines! ip)
	(make-lex-buffer ip 0 0 0 0))))
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
        (port-count-lines! ip)
	(make-lex-buffer ip 0 (car offsets) (cadr offsets) (caddr offsets)))))))

  ;; next-char: lex-buffer -> (string or eof)
  (define (next-char lb)
    (let* ((peek-amt (lex-buffer-peek-amt lb))
	   (str (peek-string 1 peek-amt (lex-buffer-ip lb))))
      (set-lex-buffer-peek-amt! lb (add1 peek-amt))
      str))

  ;; get-match: lex-buf * int -> string
  ;; reads the next i characters.
  (define (get-match lb i)
    (set-lex-buffer-peek-amt! lb 0)
    (read-string i (lex-buffer-ip lb)))

  (define-struct position (offset line col))
  (define (get-position lb)
    (let-values (((line col off) (port-next-location (lex-buffer-ip lb))))
      (if (and line col)
	  (make-position (+ (lex-buffer-offset-start lb) off)
			 (+ (lex-buffer-line-start lb) line)
			 (if (= line 1)
			     (+ (lex-buffer-col-start lb) col)
			     col))
	  (make-position (+ (lex-buffer-offset-start lb) off) #f #f))))

)
