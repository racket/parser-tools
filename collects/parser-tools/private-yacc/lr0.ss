#cs
(module lr0 mzscheme

  ;; Handle the LR0 automaton
  
  (require "grammar.ss"
	   "graph.ss"
	   (lib "list.ss"))
  
  (provide union build-lr0-automaton run-automaton (struct trans-key (st gs))
	   lr0-transitions lr0-states lr0-epsilon-trans
	   kernel-items kernel-index for-each-state)

  (define (union comp<?)
    (letrec ((union
	      (lambda (l1 l2)
		(cond
		 ((null? l1) l2)
		 ((null? l2) l1)
		 (else (let ((c1 (car l1))
			     (c2 (car l2)))
			 (cond
			  ((comp<? c1 c2)
			   (cons c1 (union (cdr l1) l2)))
			  ((comp<? c2 c1)
			   (cons c2 (union l1 (cdr l2))))
			  (else (union (cdr l1) l2)))))))))
      union))
  
  ;; kernel = (make-kernel (LR1-item list) index)
  ;;   the list must be kept sorted according to item<? so that equal? can
  ;;   be used to compare kernels
  ;; LR0-automaton = (make-lr0 (trans-key kernel hash-table) (kernel vector) (kernel item hashtable))
  ;; trans-key = (make-trans-key kernel gram-sym)
  (define-struct kernel (items index) (make-inspector))
  (define-struct trans-key (st gs) (make-inspector))
  (define-struct lr0 (transitions states epsilon-trans) (make-inspector))

  ;; Iteration over the states in an automaton
  (define (for-each-state f a)
    (let* ((states (lr0-states a))
	   (num-states (vector-length states)))
      (let loop ((i 0))
	(if (< i num-states)
	    (begin
	      (f (vector-ref states i))
	      (loop (add1 i)))))))
 
  ;; The kernels in the automaton are represented cannonically.
  ;; That is (equal? a b) <=> (eq? a b)
  (define (kernel->string k)
    (apply string-append 
	   `("{" ,@(map (lambda (i) (string-append (item->string i) ", ")) 
			(kernel-items k)) 
	     "}")))

  ;; run-automaton: kernel * gram-sym * LR0-automaton -> kernel | #f
  ;; returns the state that the transition trans-key provides or #f
  ;; if there is no such state
  (define (run-automaton k s a)
    (hash-table-get (lr0-transitions a) (make-trans-key k s) (lambda () #f)))


  ;; build-LR0-automaton: grammar -> LR0-automaton
  ;; Constructs the kernels of the sets of LR(0) items of g
  (define (build-lr0-automaton grammar)
;    (printf "LR(0) automaton:~n")
    (letrec (
	     (terms (list->vector (grammar-terms grammar)))
	     (non-terms (list->vector (grammar-non-terms grammar)))
	     (num-non-terms (vector-length non-terms))
	     (num-gram-syms (+ num-non-terms (vector-length terms)))
	     (epsilons (make-hash-table 'equal))
	     
	     ;; first-non-term: non-term -> non-term list
	     ;; given a non-terminal symbol C, return those non-terminal 
	     ;; symbols A s.t. C -> An for some string of terminals and
	     ;; non-terminals n where -> means a rightmost derivation in many 
	     ;; steps.  Assumes that each non-term can be reduced to a string 
	     ;; of terms.
	     (first-non-term 
	      (digraph (grammar-non-terms grammar)
		       (lambda (nt)
			 (filter non-term?
				 (map (lambda (prod)
					(sym-at-dot (make-item prod 0)))
				      (get-nt-prods grammar nt))))
		       (lambda (nt) (list nt))
		       (union non-term<?)
		       null))

	     ;; closure: LR1-item list -> LR1-item list
	     ;; Creates a set of items containing i s.t. if A -> n.Xm is in it,
	     ;; X -> .o is in it too.
	     (LR0-closure
	      (lambda (i)
		(cond
		 ((null? i) null)
		 (else
		  (let ((next-gsym (sym-at-dot (car i))))
		    (cond
		     ((non-term? next-gsym)
		      (cons (car i)
			    (append 
			     (apply append
				    (map (lambda (non-term) 
					   (map (lambda (x) 
						  (make-item x 0))
						(get-nt-prods grammar 
							      non-term)))
					 (first-non-term next-gsym)))
			     (LR0-closure (cdr i)))))
		     (else
		      (cons (car i) (LR0-closure (cdr i))))))))))



	     ;; maps trans-keys to kernels
	     (automaton (make-hash-table 'equal))
	     
	     ;; keeps the kernels we have seen, so we can have a unique
	     ;; list for each kernel
	     (kernels (make-hash-table 'equal))

	     (counter 1)
	     
	     ;; goto: LR1-item list -> LR1-item list list
	     ;; creates new kernels by moving the dot in each item in the
	     ;; LR0-closure of kernel to the right, and grouping them by 
	     ;; the term/non-term moved over.  Returns the kernels not
	     ;; yet seen, and places the trans-keys into automaton
	     (goto
	      (lambda (kernel)
		(let (
		      ;; maps each gram-syms to a list of items

		      (table (make-vector num-gram-syms null))
		      (epsilons (make-hash-table 'equal))

		      ;; add-item!: 
		      ;;   (item list) vector * item ->
		      ;; adds i into the table grouped with the grammar
		      ;; symbol following its dot
		      (add-item!
		       (lambda (table i)
			 (let ((gs (sym-at-dot i)))
			   (cond
			    (gs
			     (let* ((add (if (term? gs)
					     num-non-terms
					     0))
				    (already 
				     (vector-ref table 
						 (+ add 
						    (gram-sym-index gs)))))
			       (if (not (member i already))
				   (vector-set! table 
						(+ add (gram-sym-index gs))
						(cons i already)))))
			    ((= 0 (vector-length (prod-rhs (item-prod i))))
			     (let ((current (hash-table-get epsilons
							    kernel
							    (lambda () null))))
			       (hash-table-put! epsilons
						kernel
						(cons i current)))))))))
		  
		  ;; Group the items of the LR0 closure of the kernel
		  ;; by the character after the dot
		  (for-each (lambda (item)
			      (add-item! table item))
			    (LR0-closure (kernel-items kernel)))

		  
		  ;; each group is a new kernel, with the dot advanced.
		  ;; sorts the items in a kernel so kernels can be compared
		  ;; with equal? for using the table kernels to make sure
		  ;; only one representitive of each kernel is created
		  (filter 
		   (lambda (x) x)
		   (map
		    (lambda (i)
		      (let* ((gs (car i))
			     (items (cadr i))
			     (new #f)
			     (new-kernel (quicksort
					  (filter (lambda (x) x)
						  (map move-dot-right items))
					  item<?))
			     (unique-kernel (hash-table-get
					     kernels
					     new-kernel
					     (lambda () 
					       (let ((k (make-kernel
							 new-kernel
							 counter)))
						 (set! new #t)
						 (set! counter (add1 counter))
						 (hash-table-put! kernels
								  new-kernel
								  k)
						 k)))))
			(hash-table-put! automaton
					 (make-trans-key kernel gs)
					 unique-kernel)
; 			(printf "~a -> ~a on ~a~n" 
; 				(kernel->string kernel)
; 				(kernel->string unique-kernel)
;				(gram-sym-symbol gs))
			(if new
			    unique-kernel
			    #f)))
		    (let loop ((i 0))
		      (cond
		       ((< i num-non-terms)
			(let ((items (vector-ref table i)))
			  (cond
			   ((null? items) (loop (add1 i)))
			   (else
			    (cons (list (vector-ref non-terms i) items)
				  (loop (add1 i)))))))
		       ((< i num-gram-syms)
			(let ((items (vector-ref table i)))
			  (cond
			   ((null? items) (loop (add1 i)))
			   (else
			    (cons (list (vector-ref terms (- i num-non-terms))
					items)
				  (loop (add1 i)))))))
		       (else null))))))))
			
	      
	     (start (list (make-item (get-init-prod grammar) 0)))
	     (startk (make-kernel start 0))
	     (new-kernels (make-queue)))
      
      (hash-table-put! kernels start startk)
      (let loop ((old-kernels (list startk))
		 (seen-kernels null))
	(cond
	 ((and (empty-queue? new-kernels) (null? old-kernels))
	  (make-lr0 automaton (list->vector (reverse! seen-kernels)) epsilons))
	 ((null? old-kernels)
	  (loop (deq! new-kernels) seen-kernels))
	 (else 
	  (enq! new-kernels (goto (car old-kernels)))
	  (loop (cdr old-kernels) (cons (car old-kernels) seen-kernels)))))))

  (define-struct q (f l) (make-inspector))
  (define (empty-queue? q)
    (null? (q-f q)))
  (define (make-queue)
    (make-q null null))
  (define (enq! q i)
    (if (empty-queue? q)
	(let ((i (list i)))
	  (set-q-l! q i)
	  (set-q-f! q i))
	(begin
	  (set-cdr! (q-l q) (list i))
	  (set-q-l! q (cdr (q-l q))))))
  (define (deq! q)
    (begin0
     (car (q-f q))
     (set-q-f! q (cdr (q-f q)))))

)