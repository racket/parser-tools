#cs
(module table mzscheme

  ;; Routine to build the LALR table
  
  (require "grammar.ss"
	   "lr0.ss"
	   "array2d.ss"
	   "lalr.ss"
	   "parser-actions.ss"
	   (lib "list.ss"))

  (provide build-table)

  ;; print-entry: symbol * action * output-port ->
  ;; prints the action a for lookahead sym to port
  (define (print-entry sym a port)
    (let ((s "\t~a\t\t\t\t\t~a\t~a\n"))
      (cond
       ((shift? a)
	(fprintf port s sym "shift" (shift-state a)))
       ((reduce? a)
	(fprintf port s sym "reduce" (reduce-prod-num a)))
       ((accept? a)
	(fprintf port s sym "accept" ""))
       (a
	(fprintf port s sym "goto" a)))))


  ;; count: ('a -> bool) * 'a list -> num
  ;; counts the number of elements in list that satisfy pred
  (define (count pred list)
    (cond
     ((null? list) 0)
     ((pred (car list)) (+ 1 (count pred (cdr list))))
     (else (count pred (cdr list)))))


  ;; display-parser: 
  ;;   action array2d * term vector * non-term vector * kernel vector * 
  ;;     output-port ->
  ;; Prints out the parser given by table.
  (define (display-parser table terms non-terms states port)
    (let* ((num-terms (vector-length terms))
	   (num-non-terms (vector-length non-terms))
	   (num-gram-syms (+ num-terms num-non-terms))
	   (num-states (vector-length states))
	   (SR-conflicts 0)
	   (RR-conflicts 0))
      (let loop ((i 0))
	(if (< i num-states)
	    (begin
	      (fprintf port "State ~a~n" i)
	      (for-each (lambda (item)
			  (fprintf port "\t~a~n" (item->string item)))
			(kernel-items (vector-ref states i)))
	      (newline port)
	      (let loop ((j 0))
		(if (< j num-terms)
		    (begin
		      (let ((act (array2d-ref 
				  table 
				  i
				  (+ j num-non-terms))))
			(cond
			 ((list? act) 
			  (fprintf port "begin conflict:~n")
			  (if (> (count reduce? act) 1)
			      (set! RR-conflicts (add1 RR-conflicts)))
			  (if (> (count shift? act) 0)
			      (set! SR-conflicts (add1 SR-conflicts)))
			  (map (lambda (x) 
				 (print-entry 
				  (gram-sym-symbol (vector-ref terms j))
				  x
				  port))
			       act)
			  (fprintf port "end conflict~n"))
			 (act (print-entry
			       (gram-sym-symbol (vector-ref terms j))
			       act
			       port))))
		      (loop (add1 j)))))
	      
	      (newline port)

	      (let loop ((j 0))
		(if (< j num-non-terms)
		    (begin
		      (let ((s (array2d-ref table i j)))
			(if s
			    (print-entry
			     (gram-sym-symbol (vector-ref non-terms j))
			     s
			     port)))
		      (loop (add1 j)))))

	      (newline port)
	      (loop (add1 i)))))
      (if (> SR-conflicts 0)
	  (fprintf port "~a shift/reduce conflicts~n" SR-conflicts))
      (if (> RR-conflicts 0)
	  (fprintf port "~a reduce/reduce conflicts~n" RR-conflicts))))

  (define (resolve-conflicts table num-states num-terms num-non-terms)
    (letrec ((SR-conflicts 0)
	     (RR-conflicts 0)
	     (get-action
	      (lambda (entry)
		(cond
		 ((list? entry)
		  (if (> (count shift? entry) 0)
		      (set! SR-conflicts (add1 SR-conflicts)))
		  (if (> (count reduce? entry) 1)
		      (set! RR-conflicts (add1 RR-conflicts)))
		  (let loop ((current-guess (make-reduce +inf.0 -1 -1))
			     (rest entry))
		    (cond
		     ((null? rest) current-guess)
		     ((shift? (car rest)) (car rest))
		     ((< (reduce-prod-num (car rest))
			 (reduce-prod-num current-guess))
		      (loop (car rest) (cdr rest)))
		     (else (loop current-guess (cdr rest))))))
		 (else entry)))))
      (let loop ((state 0))
	(if (< state num-states)
	    (begin
	      (let loop ((term 0))
		(if (< term num-terms)
		    (begin
		      (array2d-set! table state (+ num-non-terms term)
				    (get-action
				     (array2d-ref table 
						  state 
						  (+ num-non-terms term))))
		      (loop (add1 term)))))
	      (loop (add1 state)))))
      (if (> SR-conflicts 0)
	  (fprintf (current-error-port) 
		   "~a shift/reduce conflicts~n" 
		   SR-conflicts))
      (if (> RR-conflicts 0)
	  (fprintf (current-error-port)
		   "~a reduce/reduce conflicts~n"
		   RR-conflicts))))
    


  (define (resolve-prec-conflicts table get-term get-prod
				  num-states num-terms num-non-terms)
    (let loop ((state 0))
      (if (< state num-states)
	  (begin
	    (let loop ((term 0))
	      (if (< term num-terms)
		  (begin
		    (let ((action (array2d-ref table 
					       state 
					       (+ num-non-terms term))))
		      (if (and (list? action)
			       (= 2 (length action))
			       (or (shift? (car action))
				   (shift? (cadr action))))
			  (let* ((shift (if (shift? (car action))
					    (car action)
					    (cadr action)))
				 (reduce (if (shift? (car action))
					     (cadr action)
					     (car action)))
				 (s-prec (term-prec
					  (vector-ref get-term
						      term)))
				 (r-prec (prod-prec
					  (vector-ref 
					   get-prod
					   (reduce-prod-num reduce)))))
			    (if (and s-prec r-prec)
				(array2d-set!
				 table
				 state
				 (+ num-non-terms term)
				 (cond
				  ((< (prec-num s-prec)
				      (prec-num r-prec))
				   reduce)
				  ((> (prec-num s-prec)
				      (prec-num r-prec))
				   shift)
				  ((eq? 'left (prec-assoc s-prec))
				   reduce)
				  ((eq? 'right (prec-assoc s-prec))
				   shift)
				  (else #f)))))))
		    (loop (add1 term)))))
	    (loop (add1 state))))))

  ;; In the result table the first index is the state and the second is the 
  ;; term/non-term index (with the non-terms coming first)
  ;; buile-table: grammar * string -> action2d-array
  (define (build-table g file)
    (let* ((a (build-lr0-automaton g))
	   (terms (grammar-terms g))
	   (non-terms (grammar-non-terms g))
	   (get-state (lr0-states a))
	   (get-term (list->vector terms))
	   (get-non-term (list->vector non-terms))
	   (get-prod (list->vector (grammar-prods g)))
	   (num-states (vector-length get-state))
	   (num-terms (vector-length get-term))
	   (num-non-terms (vector-length get-non-term))
	   (num-gram-syms (+ num-terms num-non-terms))
	   (table (make-array2d num-states num-gram-syms #f))
	   (array2d-add! 
	    (lambda (v i1 i2 a)
	      (let ((old (array2d-ref v i1 i2)))
		(cond
		 ((not old) (array2d-set! v i1 i2 a))
		 ((list? old) (if (not (member a old))
				  (array2d-set! v i1 i2 (cons a old))))
		 (else (if (not (equal? a old))
			   (array2d-set! v i1 i2 (list a old))))))))
	   (get-lookahead (compute-LA a g)))

      (let loop ((state 0))
	(if (< state num-states)
	    (begin
	      (let loop ((i 0))
		(if (< i num-gram-syms)
		    (begin
		      (let* ((s (if (< i num-non-terms)
				    (vector-ref get-non-term i)
				    (vector-ref get-term (- i num-non-terms))))
			     (goto
			      (run-automaton (vector-ref get-state state)
					     s
					     a)))
			(if goto
			    (array2d-set! table 
					  state 
					  i
					  (if (< i num-non-terms)
					      (kernel-index goto)
					      (make-shift 
					       (kernel-index goto))))))
		      (loop (add1 i)))))
	      (let ((items
		     (filter (lambda (item)
			       (not (move-dot-right item)))
			     (kernel-items
			      (vector-ref get-state state)))))
		(for-each
		 (lambda (item)
		   (for-each 
		    (lambda (t)
		      (array2d-add! table 
				    state
				    (+ num-non-terms (gram-sym-index t))
				    (cond
				     ((not (start-item? item))
				      (make-reduce
				       (item-prod-index item)
                                       (gram-sym-index (prod-lhs (item-prod item)))
                                       (vector-length (prod-rhs (item-prod item))))))))
		    (get-lookahead (vector-ref get-state state)
				   (item-prod item))))
		 items))
	      
	      (loop (add1 state)))))
      (resolve-prec-conflicts table get-term get-prod num-states num-terms
			      num-non-terms)
      (if (not (string=? file ""))
	  (with-handlers [(exn:i/o:filesystem?
			   (lambda (e)
			     (fprintf 
			      (current-error-port)
			      "Cannot write debug output to file \"~a\".  ~a~n"
			      (exn:i/o:filesystem-pathname e)
			      (exn:i/o:filesystem-detail e))))]
	    (call-with-output-file file
	      (lambda (port)
		(display-parser table get-term get-non-term get-state port)))))
      (resolve-conflicts table num-states num-terms num-non-terms)
      table))
)

