#cs
(module lalr mzscheme
  
  ;; Compute LALR lookaheads from DeRemer and Pennello 1982
  
  (require "lr0.ss"
	   "grammar.ss"
	   "graph.ss"
	   "array2d.ss"
	   (lib "list.ss")
	   (lib "class.ss"))

  (provide compute-LA)

  ;; compute-DR: LR0-automaton * grammar -> (trans-key -> term list)
  ;; computes for each state, non-term transition pair, the terminals
  ;; which can transition out of the resulting state
  (define (compute-DR a g)
    (lambda (tk)
      (let ((r (send a run-automaton (trans-key-st tk) (trans-key-gs tk))))
        (term-list->bit-vector
         (filter
          (lambda (term)
            (send a run-automaton r term))
          (grammar-terms g))))))
  
  ;; compute-reads: 
  ;;   LR0-automaton * grammar -> (trans-key -> trans-key list)
  (define (compute-reads a g)
    (lambda (tk)
      (let ((r (send a run-automaton (trans-key-st tk) (trans-key-gs tk))))
	(map (lambda (x) (make-trans-key r x))
	     (filter (lambda (non-term)
		       (and (nullable? g non-term)
			    (send a run-automaton r non-term)))
		     (grammar-non-terms g))))))

  ;; compute-read: LR0-automaton * grammar -> (trans-key -> term list)
  (define (compute-read a g)
    (let* ((dr (compute-DR a g))
	   (reads (compute-reads a g)))
      (digraph-tk->terml (send a get-mapped-non-term-keys)
			 reads
			 dr
			 (vector-length (send a get-states))
			 (length (grammar-terms g))
			 (length (grammar-non-terms g)))))

  
  ;; comput-includes-and-lookback: 
  ;;   lr0-automaton * grammar -> (value (trans-key -> trans-key list)
  ;;                                     (kernel * prod -> trans-key list))
  (define (compute-includes-and-lookback a g)
    (let* ((non-terms (grammar-non-terms g))
	   (num-states (vector-length (send a get-states)))
	   (num-non-terms (length non-terms))
	   (includes (make-array2d num-states num-non-terms null))
	   (lookback (make-array2d num-states
				   (grammar-num-prods g)
				   null)))
      (send a for-each-state
       (lambda (state)
         (for-each
          (lambda (non-term)
            (for-each
             (lambda (prod)
               (let loop ((i (make-item prod 0))
                          (p state))
                 (if (and p i)
                     (let* ((next-sym (sym-at-dot i))
			    (new-i (move-dot-right i)))
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
                                 (send a run-automaton p next-sym)))))))
             (get-nt-prods g non-term)))
          non-terms)))

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
      (digraph-tk->terml (send a get-mapped-non-term-keys)
			includes
			read
			(vector-length (send a get-states))
			(length (grammar-terms g))
			(length (grammar-non-terms g)))))

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
    (send a for-each-state
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
        (grammar-non-terms g))))
    (newline))

  (define (print-input-st-prod f name a g print-output)
    (printf "~a:~n" name)
    (send a for-each-state
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
        (grammar-non-terms g)))))
  
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

  ;; digraph-tk->terml: 
  ;;   (trans-key list) * (trans-key -> trans-key list) * (trans-key -> term list) * int * int * int
  ;;     -> (trans-key -> term list)
  ;; DeRemer and Pennello 1982
  ;; Computes (f x) = (f- x) union Union{(f y) | y in (edges x)}
  ;; A specialization of digraph in the file graph.ss
  (define (digraph-tk->terml nodes edges f- num-states num-terms num-non-terms)
    (letrec (
	     ;; Will map elements of trans-key to term sets represented as bit vectors
	     (results-terms (make-array2d num-states num-terms 0))
	     (results-non-terms (make-array2d num-states num-non-terms 0))

	     ;; Maps elements of trans-keys to integers.
	     (N-terms (make-array2d num-states num-terms 0))
	     (N-non-terms (make-array2d num-states num-non-terms 0))

	     (lookup-tk-map
	      (lambda (map-term map-non-term)
		(lambda (tk) 
		  (let ((st (trans-key-st tk))
			(gs (trans-key-gs tk)))
		    (if (term? gs)
			(array2d-ref map-term (kernel-index st) (term-index gs))
			(array2d-ref map-non-term (kernel-index st) (non-term-index gs)))))))
	     (add-tk-map
	      (lambda (map-term map-non-term)
		(lambda (tk v)
		  (let ((st (trans-key-st tk))
			(gs (trans-key-gs tk)))
		    (if (term? gs)
			(array2d-set! map-term (kernel-index st) (term-index gs) v)
			(array2d-set! map-non-term (kernel-index st) (non-term-index gs) v))))))
		      
	     (get-N (lookup-tk-map N-terms N-non-terms))
	     (set-N (add-tk-map N-terms N-non-terms))
	     (get-f (lookup-tk-map results-terms results-non-terms))
	     (set-f (add-tk-map results-terms results-non-terms))
	     
	     (stack null)
	     (push (lambda (x)
		     (set! stack (cons x stack))))
	     (pop (lambda () 
                    (begin0 
		     (car stack)
		     (set! stack (cdr stack)))))
       	     (depth (lambda () (length stack)))

	     ;; traverse: 'a -> 
	     (traverse
	      (lambda (x)
		(push x)
		(let ((d (depth)))
		  (set-N x d)
		  (set-f x (f- x))
		  (for-each (lambda (y)
			      (if (= 0 (get-N y))
				  (traverse y))
			      (set-f x (bitwise-ior (get-f x) (get-f y)))
			      (set-N x (min (get-N x) (get-N y))))
			    (edges x))
		  (if (= d (get-N x))
		      (let loop ((p (pop)))
			(set-N p +inf.0)
			(set-f p (get-f x))
			(if (not (equal? x p))
			    (loop (pop)))))))))
      (for-each (lambda (x)
		  (if (= 0 (get-N x))
		      (traverse x)))
		nodes)
      get-f))
)



