(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See doc.txt.
	
  (require-for-syntax (lib "list.ss")
                      (lib "define.ss" "syntax")
                      (lib "boundmap.ss" "syntax")
                      "private-lex/util.ss"
                      "private-lex/actions.ss"
                      "private-lex/front.ss"
                      "private-lex/unicode-chars.ss")

  (require (lib "stxparam.ss")
           (lib "readerr.ss" "syntax")
           "private-lex/token.ss")

  (provide lexer lexer-src-pos define-lex-abbrev define-lex-abbrevs define-lex-trans
           
           ;; Dealing with tokens and related structures 
           define-tokens define-empty-tokens token-name token-value token?
           (struct position (offset line col))
           (struct position-token (token start-pos end-pos))
           
           ;; File path for highlighting errors while lexing
           file-path
           
           ;; Lex abbrevs for unicode char sets.  See mzscheme manual section 3.4.
           any-char any-string nothing alphabetic lower-case upper-case title-case
           numeric symbolic punctuation graphic whitespace blank iso-control

           ;; A regular expression operator
           char-set)
  
  (define file-path (make-parameter #f))
             
  ;; wrap-action: syntax-object -> syntax-object
  (define-for-syntax (wrap-action action)
    (with-syntax ((action-stx action))
      (syntax/loc action
        (lambda (start-pos-p end-pos-p lexeme-p return-without-pos-p input-port-p)
          (syntax-parameterize 
           ((start-pos (lambda (x) #'start-pos-p))
            (end-pos (lambda (x) #'end-pos-p))
            (lexeme (lambda (x) #'lexeme-p))
            (return-without-pos (lambda (x) #'return-without-pos-p))
            (input-port (lambda (x) #'input-port-p)))
           action-stx)))))
        
  (define-for-syntax (make-lexer-trans wrap?)
    (lambda (stx)
      (syntax-case stx ()
        ((_)
         (raise-syntax-error #f "accepts the empty string" stx))
        ((_ re-act ...)
         (begin
           (for-each
            (lambda (x)
              (syntax-case x ()
                ((re act) (void))
                (_ (raise-syntax-error #f
                                       "not a regular expression / action pair"
                                       stx
                                       x))))
            (syntax->list (syntax (re-act ...))))
           (let* ((spec/re-act-lst
                   (syntax->list (syntax (re-act ...))))
                  (eof-act
                   (get-special-action spec/re-act-lst #'eof #''eof))
                  (spec-act 
                   (get-special-action spec/re-act-lst #'special #'(void)))
                  (spec-error-act
                   (get-special-action spec/re-act-lst #'special-error #'(raise lexeme)))
                  (spec-comment-act 
                   (get-special-action spec/re-act-lst #'special-comment #'#f))
                  (ids (list #'special #'special-comment #'special-error #'eof))
                  (re-act-lst
                   (filter
                    (lambda (spec/re-act)
                      (syntax-case spec/re-act ()
                        (((special) act)
                           (not (ormap
                                 (lambda (x)
                                   (module-identifier=? (syntax special) x))
                                 ids)))
                        (_ #t)))
                    spec/re-act-lst)))
             (let-values (((trans start actions no-look)
                           (build-lexer re-act-lst)))
               (with-syntax ((start-state-stx start)
                             (trans-table-stx trans)
                             (no-lookahead-stx no-look)
                             (actions-stx
                              `(vector ,@(map (lambda (a)
                                                (if a (wrap-action a) #f))
                                              (vector->list actions))))
                             (spec-act-stx
                              (wrap-action spec-act))
                             (spec-error-act-stx
                              (wrap-action spec-error-act))
                             (has-comment-act?-stx 
                              (if (syntax-e spec-comment-act) #t #f))
                             (spec-comment-act-stx
                              (wrap-action spec-comment-act))
                             (eof-act-stx (wrap-action eof-act))
                             (wrap? wrap?))
                 (syntax 
                  (lexer-body start-state-stx 
                              trans-table-stx
                              actions-stx
                              no-lookahead-stx
                              spec-act-stx
                              spec-error-act-stx
                              has-comment-act?-stx
                              spec-comment-act-stx
                              eof-act-stx
                              wrap?))))))))))

  (define-syntax lexer (make-lexer-trans #f))
  (define-syntax lexer-src-pos (make-lexer-trans #t))
    
  (define-syntax (define-lex-abbrev stx)
    (syntax-case stx ()
      ((_ name re)
       (identifier? (syntax name))
       (syntax
        (define-syntax name
          (make-lex-abbrev (quote-syntax re)))))
      (_ 
       (raise-syntax-error
        #f
        "form should be (define-lex-abbrev name re)"
        stx))))

  (define-syntax (define-lex-abbrevs stx)
    (syntax-case stx ()
      ((_ x ...)
       (with-syntax (((abbrev ...)
                      (map 
                       (lambda (a)
                         (syntax-case a ()
                           ((name re)
                            (identifier? (syntax name))
                            (syntax (define-lex-abbrev name re)))
                           (_ (raise-syntax-error
                               #f
                               "form should be (define-lex-abbrevs (name re) ...)"
                               stx
                               a))))
                       (syntax->list (syntax (x ...))))))
         (syntax/loc stx (begin abbrev ...))))
      (_
       (raise-syntax-error
        #f
        "form should be (define-lex-abbrevs (name re) ...)"
        stx))))

  (define-syntax (define-lex-trans stx)
    (syntax-case stx ()
      ((_ name-form body-form)
       (let-values (((name body)
                     (normalize-definition (syntax (define-syntax name-form body-form)) #'lambda)))
         #`(define-syntax #,name (make-lex-trans #,body))))
      (_
       (raise-syntax-error
        #f
        "form should be (define-lex-trans name transformer)"
        stx))))
       

  (define (get-next-state-helper char min max table)
    (if (>= min max)
        #f
        (let* ((try (quotient (+ min max) 2))
               (el (vector-ref table try))
               (range (car el))
               (r1 (car range))
               (r2 (cdr range)))
          (cond
            ((and (>= char r1) (<= char r2)) (cdr el))
            ((< char r1) (get-next-state-helper char min try table))
            (else (get-next-state-helper char (add1 try) max table))))))
               
          
          
  
  (define (get-next-state char table)
    (if table
        (get-next-state-helper char 0 (vector-length table) table)
        #f))
  
  (define (lexer-body start-state trans-table actions no-lookahead
                      special-action special-error-action
                      has-special-comment-action? special-comment-action eof-action wrap?)
    (letrec ((lexer
              (lambda (ip)
                (unless (input-port? ip)
                  (raise-type-error 
                   'lexer 
                   "input-port"
                   0
                   ip))
                (let ((first-pos (get-position ip))
                      (first-char (peek-char-or-special ip 0)))
                  ;; (printf "(peek-char-or-special port 0) = ~e~n" first-char)
                  (cond
                    ((eof-object? first-char)
                     (do-match ip first-pos eof-action (read-char-or-special ip) wrap?))
                    ((not (char? first-char))
                     (let* ((comment? #f)
                            (error? #f)
                            (spec (with-handlers ((special-comment?
                                                   (lambda (x) (set! comment? #t)))
                                                  (exn:fail?
                                                   (lambda (ex) (set! error? #t) ex)))
                                    (read-char-or-special ip))))
                       (cond
                         ((and comment? (not has-special-comment-action?))
                          (lexer ip))
                         (else
                          (do-match ip first-pos (cond
                                                   (comment? special-comment-action)
                                                   (error? special-error-action)
                                                   (else special-action))
                                    spec wrap?)))))
                    (else
                     (let lexer-loop (
                                      ;; current-state
                                      (state start-state)
                                      ;; the character to transition on
                                      (char first-char)
                                      ;; action for the longest match seen thus far
                                      ;; including a match at the current state
                                      (longest-match-action 
                                       (vector-ref actions start-state))
                                      ;; how many characters have been read
                                      ;; including the one just read
                                      (length-bytes (char-utf-8-length first-char))
                                      (length-chars 1)
                                      ;; how many characters are in the longest match
                                      (longest-match-length 1))
                       ;; (printf "(peek-char-or-special port ~e) = ~e~n" (sub1 length-bytes) char)
                       (let ((next-state 
                              (cond
                                ((eof-object? char) #f)
                                ((not (char? char)) #f)
                                (else (get-next-state (char->integer char)
                                                      (vector-ref trans-table state))))))
                         (cond
                           ((not next-state)
                            (check-match ip first-pos longest-match-length
                                         length-chars longest-match-action wrap?))
                           ((vector-ref no-lookahead next-state)
                            (let ((act (vector-ref actions next-state)))
                              (check-match ip 
                                           first-pos 
                                           (if act length-chars longest-match-length)
                                           length-chars
                                           (if act act longest-match-action)
                                           wrap?)))
                           (else
                            (let ((act (vector-ref actions next-state)))
                              (lexer-loop next-state 
                                          (peek-char-or-special ip length-bytes)
                                          (if act
                                              act
                                              longest-match-action)
                                          (+ (char-utf-8-length char) length-bytes)
                                          (add1 length-chars)
                                          (if act
                                              length-chars
                                              longest-match-length)))))))))))))
      lexer))
      
  (define id (lambda (x) x))

  (define (check-match lb first-pos longest-match-length length longest-match-action wrap?)
    (unless longest-match-action
      (let* ((match (read-string length lb))
	     (end-pos (get-position lb)))
	(raise-read-error
	 (format "lexer: No match found in input starting with: ~a" match)
	 (file-path)
	 (position-line first-pos)
	 (position-col first-pos)
	 (position-offset first-pos)
	 (- (position-offset end-pos) (position-offset first-pos)))))
    (let ((match (read-string longest-match-length lb)))
      ;; (printf "(read-string ~e port) = ~e~n" longest-match-length match)
      (do-match lb first-pos longest-match-action match wrap?)))
      
      
  (define (do-match ip first-pos action value wrap?)
    (let ((end-pos (get-position ip)))
      (cond
        (wrap?
         (let/ec ret
           (make-position-token (action first-pos end-pos value ret ip)
                                first-pos 
                                end-pos)))
        (else 
         (action first-pos end-pos value id ip)))))
  
  (define (get-position ip)
    (let-values (((line col off) (port-next-location ip)))
      (make-position off line col)))

  (define-syntax (create-unicode-abbrevs stx)
    (syntax-case stx ()
      ((_ ctxt)
       (with-syntax (((ranges ...) (map (lambda (range)
                                          `(union ,@(map (lambda (x)
                                                           `(char-range ,(integer->char (car x))
                                                                        ,(integer->char (cdr x))))
                                                         range)))
                                        (list (force alphabetic-ranges)
                                              (force lower-case-ranges)
                                              (force upper-case-ranges)
                                              (force title-case-ranges)
                                              (force numeric-ranges)
                                              (force symbolic-ranges)
                                              (force punctuation-ranges)
                                              (force graphic-ranges)
                                              (force whitespace-ranges)
                                              (force blank-ranges)
                                              (force iso-control-ranges))))
                     ((names ...) (map (lambda (sym)
                                         (datum->syntax-object (syntax ctxt) sym #f))
                                       '(alphabetic
                                         lower-case
                                         upper-case
                                         title-case
                                         numeric
                                         symbolic
                                         punctuation
                                         graphic
                                         whitespace
                                         blank
                                         iso-control))))
         (syntax (define-lex-abbrevs (names ranges) ...))))))
                                             
  (define-lex-abbrev any-char (char-complement (union)))
  (define-lex-abbrev any-string (intersection))
  (define-lex-abbrev nothing (union))
  (create-unicode-abbrevs #'here)
  
  (define-lex-trans (char-set stx)
                    (syntax-case stx ()
                      ((_ str)
       (string? (syntax-e (syntax str)))
       (with-syntax (((char ...) (string->list (syntax-e (syntax str)))))
         (syntax (union char ...))))))

  (define-syntax provide-lex-keyword
    (syntax-rules ()
      [(_ id ...)
       (begin
	 (define-syntax-parameter id
           (make-set!-transformer
            (lambda (stx)
              (raise-syntax-error
               #f
               (format "use of a lexer keyword (~a) is not in an appropriate lexer action"
                       'id)
               stx))))
	 ...
	 (provide id ...))]))
  
  (provide-lex-keyword start-pos end-pos lexeme input-port return-without-pos)

)
