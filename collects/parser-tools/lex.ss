(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See doc.txt.
	
  (require-for-syntax "private-lex/generate-code.ss")
  (require-for-syntax "private-lex/structs.ss")
  (require (lib "list.ss"))
  (provide lex define-lex-abbrev define-lex-abbrevs
	   make-lex-buf
	   get-position position-offset position-line position-col position?)


  (define-syntax lex
    (let ((code
	   `(letrec ((match
		      (lambda (lb first-pos longest-match-length longest-match-action length)
			(let ((match 
			       (push-back lb (- length longest-match-length))))
			  (if (not longest-match-action)
			      (error 'lex "No match found in rest of input"))
			  (longest-match-action
			   (lambda ()
			     first-pos)
			   (lambda ()
			     (get-position lb))
			   (lambda ()
			     (list->string (reverse (filter (lambda (x) 
							      (char? x))
							    match))))
			   lb)))))
	      (lambda (lb)
		(unless (lex-buffer? lb)
			(error 'lex 
			       (format 
				"Lexer expects argument of type lex-buf; given ~a" lb)))
		(let ((first-pos (get-position lb)))
		  (let loop (
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
			(loop next-state 
			      (next-char lb)
			      (if (vector-ref actions next-state)
				  (vector-ref actions next-state)
				  longest-match-action)
			      (add1 length)
			      (if (vector-ref actions next-state)
				  length
				  longest-match-length)))))))))))
      (lambda (stx)
	(syntax-case stx ()
		     ((_ (re act) ...) 
		      (let* ((table (generate-table (syntax ((re act) ...)) 
						    stx))
			     (code
			      `(let ((start-state ,(table-start table))
				     (trans-table ,(table-trans table))
				     (eof-table ,(table-eof table))
				     (actions (vector ,@(vector->list (table-actions table)))))
				 ,code)))
			(datum->syntax-object #'here code #f)))))))
  


  (define-syntax define-lex-abbrev
    (syntax-rules ()
      ((_ name val) (define-syntax name
		      (make-lex-abbrev (quote-syntax val))))))

  (define-syntax define-lex-abbrevs
    (syntax-rules ()
      ((_ (name val) ...)
       (define-syntaxes (name ...)
	 (values (make-lex-abbrev (quote-syntax val)) ...)))))


  ;; Lex buffer is NOT thread safe


  ;; c = char | eof
  ;; lex-buf = 
  ;;   (make-lex-buffer input-port (c list) (c list) int int int (int list))
  (define-struct lex-buffer (ip from to offset line col line-lengths))

  ;; make-lex-buf: input-port -> lex-buf
  (define (make-lex-buf ip)
    (make-lex-buffer ip null null 0 0 0 null))

  ;; next-char: lex-buf -> c
  ;; gets the next character from the buffer
  (define (next-char lb)
    (let ((char-in
	   (cond
	    ((null? (lex-buffer-from lb)) 
	     (read-char (lex-buffer-ip lb)))
	    (else (begin0
		   (car (lex-buffer-from lb))
		   (set-lex-buffer-from! lb (cdr (lex-buffer-from lb))))))))
      (set-lex-buffer-to! lb (cons char-in (lex-buffer-to lb)))
      (cond 
       ((eq? #\newline char-in)
	(set-lex-buffer-line-lengths!
	 lb
	 (cons (lex-buffer-col lb)
	       (lex-buffer-line-lengths lb)))
	(set-lex-buffer-line! lb (add1 (lex-buffer-line lb)))
	(set-lex-buffer-col! lb 0))
       (else
	(set-lex-buffer-col! lb (add1 (lex-buffer-col lb)))))
      (set-lex-buffer-offset! lb (add1 (lex-buffer-offset lb)))
      char-in))
  
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
		    (set-lex-buffer-line! 
		     lb
		     (sub1 (lex-buffer-line lb)))
		    (set-lex-buffer-offset!
		     lb
		     (car (lex-buffer-line-lengths lb)))
		    (set-lex-buffer-line-lengths!
		     lb
		     (cdr (lex-buffer-line-lengths lb))))
		   (else
		    (set-lex-buffer-col! 
		     lb 
		     (sub1 (lex-buffer-col lb)))))
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
		  ret)))

  (define-struct position (offset line col))
  (define (get-position lb)
    (make-position (lex-buffer-offset lb)
		   (lex-buffer-line lb)
		   (lex-buffer-col lb)))


)







