#cs
(module lalr mzscheme
  
  ;; Compute LALR lookaheads from DeRemer and Pennello 1982
  
  (require "lr0.ss"
	   "grammar.ss"
	   "graph.ss"
	   "array2d.ss"
	   (lib "list.ss"))

  (provide compute-LA)

  ;; compute-DR: LR0-automaton * grammar -> (trans-key -> term list)
  ;; computes for each state, non-term transition pair, the terminals
  ;; which can transition out of the resulting state
  (define (compute-DR a g)
    (lambda (tk)
      (let ((r (run-automaton (trans-key-st tk) (trans-key-gs tk) a)))
        (term-list->bit-vector
         (filter
          (lambda (term)
            (run-automaton r term a))
          (grammar-terms g))))))
  
  ;; compute-reads: 
  ;;   LR0-automaton * grammar -> (trans-key -> trans-key list)
  (define (compute-reads a g)
    (lambda (tk)
      (let ((r (run-automaton (trans-key-st tk) (trans-key-gs tk) a)))
	(map (lambda (x) (make-trans-key r x))
	     (filter (lambda (non-term)
		       (and (nullable? g non-term)
			    (run-automaton r non-term a)))
		     (grammar-non-terms g))))))

  ;; compute-read: LR0-automaton * grammar -> (trans-key -> term list)
  (define (compute-read a g)
    (let* ((dr (compute-DR a g))
	   (reads (compute-reads a g)))
      (digraph (get-mapped-lr0-non-term-keys a)
	       reads
	       dr
	       bitwise-ior
	       (lambda () 0))))

  
  ;; comput-includes-and-lookback: 
  ;;   lr0-automaton * grammar -> (value (trans-key -> trans-key list)
  ;;                                     (kernel * prod -> trans-key list))
  (define (compute-includes-and-lookback a g)
    (let* ((non-terms (grammar-non-terms g))
	   (num-states (vector-length (lr0-states a)))
	   (num-non-terms (length non-terms))
	   (includes (make-array2d num-states num-non-terms null))
	   (lookback (make-array2d num-states
				   (grammar-num-prods g)
				   null)))
      (for-each-state
       (lambda (state)
         (for-each
          (lambda (non-term)
            (for-each
             (lambda (prod)
               (let loop ((i (make-item prod 0))
                          (p state))
                 (if (and p i)
                     (let ((new-i (move-dot-right i))
                           (next-sym (sym-at-dot i)))
                       (if (and (non-term? next-sym)
                                (nullable-after-dot? new-i g))
                           (array2d-add! includes
                                         (kernel-index p)
                                         (gram-sym-index next-sym)
                                         (make-trans-key state non-term)))
                       (if (not new-i)
                           (array2d-add! lookback
                                         (kernel-index p)
                                         (prod-index prod)
                                         (make-trans-key state non-term)))
                       (if next-sym
                           (loop new-i
                                 (run-automaton p next-sym a)))))))
             (get-nt-prods g non-term)))
          non-terms))
       a)
      (values (lambda (tk)
		(array2d-ref includes 
			     (kernel-index (trans-key-st tk))
			     (gram-sym-index (trans-key-gs tk))))
	      (lambda (state prod)
		(array2d-ref lookback
			     (kernel-index state)
			     (prod-index prod))))))


  ;; compute-follow:  LR0-automaton * grammar -> (trans-key -> term list)
  (define (compute-follow a g includes)
    (let ((read (compute-read a g)))
      (digraph (get-mapped-lr0-non-term-keys a)
	       includes
	       read
	       bitwise-ior
	       (lambda () 0))))

  ;; compute-LA: LR0-automaton * grammar -> (kernel * prod -> term list)
  (define (compute-LA a g)
    (let-values (((includes lookback) (time (compute-includes-and-lookback a g))))
      (let ((follow (time (compute-follow a g includes))))
	(lambda (k p)
	  (let* ((l (lookback k p))
                 (f (map follow l)))
	    (apply bitwise-ior (cons 0 f)))))))


  (define (print-DR dr a g)
    (print-input-st-sym dr "DR" a g print-output-terms))
  (define (print-Read Read a g)
    (print-input-st-sym Read "Read" a g print-output-terms))
  (define (print-includes i a g)
    (print-input-st-sym i "includes" a g print-output-st-nt))
  (define (print-lookback l a g)
    (print-input-st-prod l "lookback" a g print-output-st-nt))
  (define (print-follow f a g)
    (print-input-st-sym f "follow" a g print-output-terms))
  (define (print-LA l a g)
    (print-input-st-prod l "LA" a g print-output-terms))

  (define (print-input-st-sym f name a g print-output)
    (printf "~a:~n" name)
    (for-each-state
     (lambda (state)
       (for-each
        (lambda (non-term)
          (let ((res (f (make-trans-key state non-term))))
            (if (not (null? res))
                (printf "~a(~a, ~a) = ~a~n"
                        name
                        state
                        (gram-sym-symbol non-term)
                        (print-output res)))))
        (grammar-non-terms g)))
     a)
    (newline))

  (define (print-input-st-prod f name a g print-output)
    (printf "~a:~n" name)
    (for-each-state
     (lambda (state)
       (for-each
        (lambda (non-term)
          (for-each
           (lambda (prod)
             (let ((res (f state prod)))
               (if (not (null? res))
                   (printf "~a(~a, ~a) = ~a~n"
                           name
                           (kernel-index state)
                           (prod-index prod)
                           (print-output res)))))
           (get-nt-prods g non-term)))
        (grammar-non-terms g)))
     a))
  
  (define (print-output-terms r)
    (map 
     (lambda (p)
       (gram-sym-symbol p))
     r))
  
  (define (print-output-st-nt r)
    (map
     (lambda (p)
       (list
	(kernel-index (trans-key-st p))
	(gram-sym-symbol (trans-key-gs p))))
     r))


)



