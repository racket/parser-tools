;; This is based on the calculator example in the bison manual.


;; Import the parser and lexer generators.
(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools"))

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG))

;; A hash table to store variable values in for the calculator
(define vars (make-hash-table))

(define-lex-abbrevs
 (lower-letter (- a z))

 ;; In the following line if we used (- A Z) dr/mzscheme would read it as (- a z) if
 ;; case-sensitivity is not enabled.  (- #\A #\Z) will not be altered because dr/mzscheme
 ;; reads them as character constants and not as symbols.  (- "A" "Z") would work as well
 ;; since dr/mzscheme would read them as strings.  The lexer generator treates single character
 ;; strings and symbols the same as an actual #\ character.  #cs(- A Z) works too because the #cs
 ;; tells dr/mzscheme to read the next expression with case-sensitivity turned on.
 (upper-letter (- #\A #\Z))

 ;; (- 0 9) would not work because the lexer does not understand numbers.  (- #\0 #\9) is ok too.
 (digit (- "0" "9")))
 
(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(: #\tab #\space) (calcl lex-buf)]
   ;; The parser will treat the return of 'newline the same as (token-newline)
   [#\newline 'newline]
   [(: = + - * / ^) (string->symbol (get-lexeme))]
   ["(" 'OP]
   [")" 'CP]
   [sin (token-FNCT sin)]
   ;; It the parens are left off of an "invocation" of an abbreviation, it means the
   ;; character sequence instead.
   [(+ (: (lower-letter) (upper-letter))) (token-VAR (string->symbol (get-lexeme)))]
   [(+ (digit)) (token-NUM (string->number (get-lexeme)))]
   ;; Strings which dr/mzscheme does not think of as symbols (such as . or ,) must be
   ;; entered as a string or character.  "." would also be ok.
   [(@ (+ (digit)) #\. (* (digit))) (token-NUM (string->number (get-lexeme)))]))
   

(define calcp
  (parser
   
   (start exp)
   (end EOF newline)
   (tokens value-tokens op-tokens)
   (error void)
   
   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   
   (grammar
    
    (exp [(NUM) $1]
         [(VAR) (hash-table-get vars $1 (lambda () 0))]
         [(VAR = exp) (begin (hash-table-put! vars $1 $3)
                             $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (+ $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))
           
;; run the calculator on the given input-port       
(define (calc ip)
  ;; Make the lex-buffer
  (let ((lb (make-lex-buf ip)))
    (calcp (lambda () (calcl lb)))))