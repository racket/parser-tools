#cs
(module read mzscheme

  (require (lib "lex.ss" "parser-tools")
           (lib "util.ss" "parser-tools"))

  (define scheme-lexer
    (lexer
     [(: (whitespace) (comment)) (scheme-lexer lex-buf)]
     ["#t" #t]
     ["#f" #f]
     [(@ "#\\" (any)) (caddr (string->list (get-lexeme)))]
     ["#\\space" #\space]
     ["#\\newline" #\newline]
     [(: (@ (initial) (* (subsequent))) + - "...") (string->symbol (get-lexeme))]
     [#\" (list->string (get-string-token lex-buf))]
     [#\( (make-OPEN-LIST)]
     [#\) (make-CLOSE)]
     ["#(" (make-OPEN-VECTOR)]
     [(num2) (string->number (get-lexeme) 2)]
     [(num8) (string->number (get-lexeme) 8)]
     [(num10) (string->number (get-lexeme) 10)]
     [(num16) (string->number (get-lexeme) 16)]
     ["'" (make-QUOTE)]
     ["`" (make-QUASI-QUOTE)]
     ["," (make-UNQUOTE)]
     [",@" (make-UNQUOTE-SPLICING)]
     ["." (make-DOT)]
     [(eof) eof]))
   
  (define get-string-token
    (lexer
     [(^ #\" #\\) (cons (car (string->list (get-lexeme)))
			(get-string-token lex-buf))]
     [(@ #\\ #\\) (cons #\\ (get-string-token lex-buf))]
     [(@ #\\ #\") (cons #\" (get-string-token lex-buf))]
     [#\" null]))
  
  
  (define-lex-abbrevs
   [initial (: (letter) ! $ % & * / : < = > ? ^ _ ~)]
   [subsequent (: (initial) (digit) + - #\. @)]
   [comment (@ #\; (* (^ #\newline)) #\newline)]
   
;   [numR (@ (prefixR) (complexR))]
;   [complexR (: (realR) 
;                (@ (realR) @ (realR))
;                (@ (realR) + (urealR) i)
;                (@ (realR) - (urealR) i)
;                (@ (realR) + i)
;                (@ (realR) - i)
;                (@ + (urealR) i)
;                (@ - (urealR) i)
;                (@ + i)
;                (@ - i))]
;   [realR (@ (sign) (urealR))]
;   [urealR (: (uintegerR) (@ (uintegerR) / (uintegerR)) (decimalR))]
;   [uintegerR (@ (+ (digitR)) (* #\#))]
;   [prefixR (: (@ (radixR) (exactness))
;               (@ (exactness) (radixR)))]
;   [numR (@ (prefixR) (complexR))]
;   [complexR (: (realR) 
;                (@ (realR) @ (realR))

   [num2 (@ (prefix2) (complex2))]
   [complex2 (: (real2) 
                (@ (real2) @ (real2))
                (@ (real2) + (ureal2) i)
                (@ (real2) - (ureal2) i)
                (@ (real2) + i)
                (@ (real2) - i)
                (@ + (ureal2) i)
                (@ - (ureal2) i)
                (@ + i)
                (@ - i))]
   [real2 (@ (sign) (ureal2))]
   [ureal2 (: (uinteger2) (@ (uinteger2) / (uinteger2)))]
   [uinteger2 (@ (+ (digit2)) (* #\#))]
   [prefix2 (: (@ (radix2) (exactness))
               (@ (exactness) (radix2)))]
   [radix2 "#b"]
   [digit2 (: #\0 #\1)]
   
   [num8 (@ (prefix8) (complex8))]
   [complex8 (: (real8) 
                (@ (real8) @ (real8))
                (@ (real8) + (ureal8) i)
                (@ (real8) - (ureal8) i)
                (@ (real8) + i)
                (@ (real8) - i)
                (@ + (ureal8) i)
                (@ - (ureal8) i)
                (@ + i)
                (@ - i))]
   [real8 (@ (sign) (ureal8))]
   [ureal8 (: (uinteger8) (@ (uinteger8) / (uinteger8)))]
   [uinteger8 (@ (+ (digit8)) (* #\#))]
   [prefix8 (: (@ (radix8) (exactness))
               (@ (exactness) (radix8)))]
   [radix8 "#o"]
   [digit8 (- #\0 #\7)]
   
   [num10 (@ (prefix10) (complex10))]
   [complex10 (: (real10) 
                (@ (real10) @ (real10))
                (@ (real10) + (ureal10) i)
                (@ (real10) - (ureal10) i)
                (@ (real10) + i)
                (@ (real10) - i)
                (@ + (ureal10) i)
                (@ - (ureal10) i)
                (@ + i)
                (@ - i))]
   [real10 (@ (sign) (ureal10))]
   [ureal10 (: (uinteger10) (@ (uinteger10) / (uinteger10)) (decimal10))]
   [uinteger10 (@ (+ (digit10)) (* #\#))]
   [prefix10 (: (@ (radix10) (exactness))
               (@ (exactness) (radix10)))]
   [radix10 (: (@) "#d")]
   [digit10 (digit)]
   [decimal10 (: (@ (uinteger10) (suffix))
                 (@ #\. (+ (digit10)) (* #\#) (suffix))
                 (@ (+ (digit10)) #\. (* (digit10)) (* #\#) (suffix))
                 (@ (+ (digit10)) (+ #\#) #\. (* #\#) (suffix)))]
   
   [num16 (@ (prefix16) (complex16))]
   [complex16 (: (real16) 
                (@ (real16) @ (real16))
                (@ (real16) + (ureal16) i)
                (@ (real16) - (ureal16) i)
                (@ (real16) + i)
                (@ (real16) - i)
                (@ + (ureal16) i)
                (@ - (ureal16) i)
                (@ + i)
                (@ - i))]
   [real16 (@ (sign) (ureal16))]
   [ureal16 (: (uinteger16) (@ (uinteger16) / (uinteger16)))]
   [uinteger16 (@ (+ (digit16)) (* #\#))]
   [prefix16 (: (@ (radix16) (exactness))
               (@ (exactness) (radix16)))]
   [radix16 "#x"]
   [digit16 (: (digit) (- #\a #\f) (- #\A #\F))]
    
   
   [suffix (: (@) (@ (exponent-marker) (sign) (+ (digit10))))]
   [exponent-marker (: e s f d l)]
   [sign (: (@) + -)]
   [exactness (: (@) "#i" "#e")]
   )
   

  (define r (build-reader scheme-lexer))
  (provide r)

  (define (compare s1 s2)
    (for-each (lambda (x y)
		(if (not (equal? x y))
		    (printf "~a~n~n~a" x y)))
	      s1 s2))
  
  (define (read-all read)
    (lambda (ip)
      (let ((r (read ip)))
	(cond
	 ((eof-object? r)
	  null)
	 (else
	  (cons r ((read-all read) ip)))))))

  (define (lex-all lexer)
    (lambda (in)
      (let ((lb (make-lex-buf in)))
	(let loop ((t (lexer lb)))
	  (if (not (eof-object? t))
	      (loop (lexer lb)))))))


  (require (lib "list.ss"))
  (define files (filter (lambda (x) 
			(string=? ".scm"
				  (substring x 
					     (- (string-length x) 4)
					     (string-length x))))
		      (directory-list)))
  (define (test)
    (printf "just lexing~n")
    (time
     (map (lambda (x)
	    (display x)
	    (newline)
	    (call-with-input-file x (lex-all scheme-lexer)))
	  files))
    #|
    (printf "reading~n")
    (time
     (map (lambda (x) 
	    (display x)
	    (newline)
	    (call-with-input-file x (read-all r)))
	  files))
    (printf "builtin read~n")
    (time
     (map (lambda (x) 
	    (display x)
	    (newline)
	    (call-with-input-file x (read-all read)))
	  files))
    (printf "testing~n")
    (for-each (lambda (x)
		(display x)
		(newline)
		(compare (call-with-input-file x (read-all read ))
			 (call-with-input-file x (read-all r))))
	      files)|#)
  
  (provide test)

  )