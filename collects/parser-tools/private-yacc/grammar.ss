#cs
(module grammar mzscheme
  
  ;; Constructs to create and access grammars, the internal
  ;; representation of the input to the parser generator.
    
  (provide 
     
   (rename export-make-item make-item)
   make-term
   make-non-term
   make-prec
   make-prod
   (rename make-gram make-grammar)
   
   ;; Things that work on items
   start-item? item-prod item-prod-index item->string 
   sym-at-dot move-dot-right item<? nullable-after-dot?

   ;; Things that operate on grammar symbols
   gram-sym-symbol gram-sym-index term-prec gram-sym->string
   non-term? term? nullable? non-term<? term<? 
   
   ;; Things that work on precs
   prec-num prec-assoc

   ;;Things that work on grammars
   get-nt-prods get-init-prod 
   (rename gram-non-terms grammar-non-terms) 
   (rename gram-terms grammar-terms)
   (rename gram-num-prods grammar-num-prods)
   (rename gram-prods grammar-prods)

   ;; Things that work on productions
   prod-index prod-prec prod-rhs prod-lhs prod-action)


  ;;---------------------- LR items --------------------------
  
  ;; LR-item = (make-item production nat (int | #f))
  ;; The n field contains the least integer such the item is nullable
  ;; after the dot if the dot is to the right of the nth position.
  (define-struct item (prod dot-pos n))

  (define (export-make-item a b)
    (make-item a b #f))
  
  (define (item-prod-index x)
    (prod-index (item-prod x)))
  
  ;; item<?: LR-item * LR-item -> bool
  ;; Lexicographic comparison on two items.
  (define (item<? i1 i2)
    (let ((p1 (prod-index (item-prod i1)))
	  (p2 (prod-index (item-prod i2))))
      (or (< p1 p2)
	  (and (= p1 p2)
	       (let ((d1 (item-dot-pos i1))
		     (d2 (item-dot-pos i2)))
		 (< d1 d2))))))

  (define (start-item? i)
    (= 0 (non-term-index (prod-lhs (item-prod i)))))

  
  ;; move-dot-right: LR-item -> LR-item | #f
  ;; moves the dot to the right in the item, unless it is at its 
  ;; rightmost, then it returns false
  (define (move-dot-right i)
    (cond
     ((= (item-dot-pos i) (vector-length (prod-rhs (item-prod i)))) #f)
     (else (make-item (item-prod i)
		      (add1 (item-dot-pos i))
		      (item-n i)))))
  
  ;; sym-at-dot: LR-item -> gram-sym | #f
  ;; returns the symbol after the dot in the item or #f if there is none  
  (define (sym-at-dot i)
    (cond
     ((= (item-dot-pos i) (vector-length (prod-rhs (item-prod i)))) #f)
     (else (vector-ref (prod-rhs (item-prod i)) (item-dot-pos i)))))
  
  ;; nullable-after-dot?: LR1-iten * grammar -> bool
  ;; determines if the string after the dot is nullable
  (define (nullable-after-dot? i g)
    (cond
     ((item-n i) => (lambda (x) (>= (item-dot-pos i) x)))
     (else
      (let ((str (prod-rhs (item-prod i))))
	(let loop ((c (sub1 (vector-length str))))
	  (cond
	   ((= c -1) (set-item-n! i 0))
	   ((term? (vector-ref str c)) (set-item-n! i (add1 c)))
	   ((nullable? g (vector-ref str c)) (loop (sub1 c)))
	   (else (set-item-n! i (add1 c))))))
      (>= (item-dot-pos i) (item-n i)))))
	

  ;; print-item: LR-item ->
  (define (item->string it)
    (let ((print-sym (lambda (i)
		       (let ((gs (vector-ref (prod-rhs (item-prod it)) i)))
			 (cond
			  ((term? gs) (format "~a " (term-sym gs)))
			  (else (format "~a " (non-term-sym gs))))))))
      (string-append
       (format "~a -> " (non-term-sym (prod-lhs (item-prod it))))
       (let loop ((i 0))
	 (cond
	  ((= i (vector-length (prod-rhs (item-prod it)))) 
	   (if (= i (item-dot-pos it))
	       ". "
	       ""))
	  ((= i (item-dot-pos it)) 
	   (string-append ". " (print-sym i) (loop (add1 i))))
	  (else (string-append (print-sym i) (loop (add1 i)))))))))

  ;; --------------------- Grammar Symbols --------------------------

  ;; gram-sym = (make-term symbol int prec)
  ;;          | (make-non-term symbol int)
  (define-struct term (sym index prec))
  (define-struct non-term (sym index))

  (define (non-term<? nt1 nt2)
    (< (non-term-index nt1) (non-term-index nt2)))
  
  (define (term<? nt1 nt2)
    (< (term-index nt1) (term-index nt2)))

  (define (gram-sym-index gs)
    (cond
     ((term? gs) (term-index gs))
     (else (non-term-index gs))))

  (define (gram-sym-symbol gs)
    (cond
     ((term? gs) (term-sym gs))
     (else (non-term-sym gs))))

  (define (gram-sym->string gs)
    (symbol->string (gram-sym-symbol gs)))

  ;; ------------------------- Precedences ---------------------------
  
  ;; a precedence declaration.  the sym should be 'left 'right or 'nonassoc
  ;; prec = (make-prec int sym)
  ;;      | #f
  (define-struct prec (num assoc))

  ;; ------------------------- Grammar ------------------------------
  
  ;; grammar = (make-gram (production list vector) 
  ;;                      (production list)
  ;;                      (bool vector)
  ;;                      (non-term list)
  ;;                      (term list)
  ;;                      int)
  ;; 
  ;; The nt-prods field is indexed by the number assigned to the non-term and
  ;;   contains the list of productions for that non-term
  ;; The prods field contains a list of all productions
  ;; The nulls field is indexed by the index for a non-term and is trus iff 
  ;;   the non-term is nullable
  (define-struct gram 
    (nt-prods prods nulls non-terms terms num-prods))


  ;; get-nt-prods: grammar * non-term -> production list
  ;; returns the productions for the given non-term
  (define (get-nt-prods g nt)
    (vector-ref (gram-nt-prods g) (non-term-index nt)))
  
  
  ;; get-init-prod: grammar -> production
  ;; gets the starting production
  (define (get-init-prod g)
    (car (vector-ref (gram-nt-prods g) 0)))

  

  (define (nullable? g nt)
    (vector-ref (gram-nulls g) (non-term-index nt)))


  ;; ------------------------ Productions ---------------------------
  
  ;; production = (make-prod non-term (gram-sym vector) int prec syntax-object)
  (define-struct prod (lhs rhs index prec action))
)
