#cs
(module lalr mzscheme
  
  ;; Compute LALR lookaheads from DeRemer and Pennello 1982
  
  (require "lr0.ss"
	   "grammar.ss"
	   "graph.ss"
	   "array2d.ss"
	   (lib "list.ss"))

  (provide compute-LA)

  (define (array2d-add! a i1 i2 v)
    (let ((old (array2d-ref a i1 i2)))
      (array2d-set! a i1 i2 (cons v old))))
  
  ;; compute-DR: LR0-automaton * grammar -> (trans-key -> term list)
  ;; computes for each state, non-term transition pair, the terminals
  ;; which can transition out of the resulting state
  (define (compute-DR a g)
    (lambda (tk)
      (let ((r (run-automaton (trans-key-st tk) (trans-key-gs tk) a)))
	(filter
	 (lambda (term)
	   (run-automaton r term a))
	 (grammar-terms g)))))
  
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
      (digraph (filter (lambda (x) (non-term? (trans-key-gs x)))
		       (hash-table-map (lr0-transitions a) (lambda (k v) k)))
	       reads
	       dr
	       (union term<?)
	       null)))

  
  ;; comput-includes-and-lookback: 
  ;;   lr0-automaton * grammar -> (value (trans-key -> trans-key list)
  ;;                                     (kernel * prod -> trans-key list))
  (define (compute-includes-and-lookback a g)
    (let* ((states (lr0-states a))
	   (non-terms (grammar-non-terms g))
	   (num-states (vector-length states))
	   (num-non-terms (length non-terms))
	   (includes (make-array2d num-states num-non-terms null))
	   (lookback (make-array2d num-states
				   (grammar-num-prods g)
				   null)))
      (let loop ((state 0))
	(if (< state num-states)
	    (begin
	      (for-each
	       (lambda (non-term)
		 (for-each
		  (lambda (prod)
		    (let loop ((i (make-item prod 0))
			       (p (vector-ref states state)))
		      (if (and p i)
			  (begin
			    (if (and (non-term? (sym-at-dot i))
				     (nullable-after-dot? (move-dot-right i)
							  g))
				(array2d-add! includes
					      (kernel-index p)
					      (gram-sym-index 
					       (sym-at-dot i))
					      (make-trans-key
					       (vector-ref states state)
					       non-term)))
			    (if (not (move-dot-right i))
				(array2d-add! lookback
					      (kernel-index p)
					      (prod-index prod)
					      (make-trans-key
					       (vector-ref states state)
					       non-term)))
			    (loop (move-dot-right i)
				  (run-automaton p (sym-at-dot i) a))))))
		  (get-nt-prods g non-term)))
	       non-terms)
	      (loop (add1 state)))))
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
      (digraph (filter (lambda (x) (non-term? (trans-key-gs x)))
		       (hash-table-map (lr0-transitions a) (lambda (k v) k)))
	       includes
	       read
	       (union term<?)
	       null)))

  ;; compute-LA: LR0-automaton * grammar -> (kernel * prod -> term list)
  (define (compute-LA a g)
    (let-values (((includes lookback) (compute-includes-and-lookback a g)))
      (let ((follow (compute-follow a g includes)))
	(lambda (k p)
	  (let* ((l (lookback k p))
		 (f (map follow l)))
	    (apply append f))))))
  
)