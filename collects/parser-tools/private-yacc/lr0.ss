#cs
(module lr0 mzscheme

  ;; Handle the LR0 automaton
  
  (require "grammar.ss"
	   "graph.ss"
           "array2d.ss"
	   (lib "list.ss")
	   (lib "class.ss"))
  
  (provide build-lr0-automaton lr0%
	   (struct trans-key (st gs)) trans-key-list-remove-dups
	   kernel-items kernel-index)
  
  ;; kernel = (make-kernel (LR1-item list) index)
  ;;   the list must be kept sorted according to item<? so that equal? can
  ;;   be used to compare kernels
  ;;   Each kernel is assigned a uniqui index, 0 <= index < number of states
  ;; trans-key = (make-trans-key kernel gram-sym)
  (define-struct kernel (items index) (make-inspector))
  (define-struct trans-key (st gs) (make-inspector))

  (define (trans-key<? a b)
    (let ((kia (kernel-index (trans-key-st a)))
          (kib (kernel-index (trans-key-st b))))
    (or (< kia kib)
        (and (= kia kib)
             (< (non-term-index (trans-key-gs a))
                (non-term-index (trans-key-gs b)))))))
  
  (define (trans-key-list-remove-dups tkl)
    (let loop ((sorted (quicksort tkl trans-key<?)))
      (cond
        ((null? sorted) null)
        ((null? (cdr sorted)) sorted)
        (else
         (if (and (= (non-term-index (trans-key-gs (car sorted)))
                     (non-term-index (trans-key-gs (cadr sorted))))
                  (= (kernel-index (trans-key-st (car sorted)))
                     (kernel-index (trans-key-st (cadr sorted)))))
             (loop (cdr sorted))
             (cons (car sorted) (loop (cdr sorted))))))))
             
      
           
  ;; kernel-list-remove-duplicates
  ;; LR0-automaton = object of class lr0%
  (define lr0%
    (class object%
      (super-instantiate ())
      ;; Hash tables that map a trans-keys to a kernel
      (init term-hash non-term-hash)
      (init-field states epsilons num-terms num-non-terms)
      
      (define term-transitions (make-lr0-table term-hash (vector-length states) num-terms #f))
      (define non-term-transitions (make-lr0-table non-term-hash (vector-length states) num-non-terms #f))
      
      (define reverse-term-hash (reverse-hash term-hash))
      (define reverse-non-term-hash (reverse-hash non-term-hash))
      (define reverse-term-transitions (make-lr0-table reverse-term-hash (vector-length states) num-terms null))
      (define reverse-non-term-transitions (make-lr0-table reverse-non-term-hash (vector-length states) num-non-terms null))
      
      (define mapped-non-terms
	(hash-table-map non-term-hash (lambda (k v) k)))
      
      (define reverse-mapped-non-terms
	(hash-table-map reverse-non-term-hash (lambda (k v) k)))
      
      (define/public (get-mapped-non-term-keys)
	mapped-non-terms)

      (define/public (get-states)
	states)

      (define/public (get-num-states)
        (vector-length states))
      
      (define/public (get-epsilon-trans)
	epsilons)

      ;; Iteration over the states in an automaton
      (define/public (for-each-state f)
	(let ((num-states (vector-length states)))
	  (let loop ((i 0))
	    (if (< i num-states)
		(begin
		  (f (vector-ref states i))
		  (loop (add1 i)))))))
      
      ;; run-automaton: kernel * gram-sym -> kernel | #f
      ;; returns the state that the transition trans-key provides or #f
      ;; if there is no such state
      (define/public (run-automaton k s)
	(if (term? s)
	    (array2d-ref term-transitions (kernel-index k) (term-index s))
	    (array2d-ref non-term-transitions (kernel-index k) (non-term-index s))))
      
      (define/public (run-automaton-back k s)
	(apply append
	       (if (term? s)
		   (map (lambda (k)
			  (array2d-ref reverse-term-transitions (kernel-index k) (term-index s)))
			k)
		   (map (lambda (k)
			  (array2d-ref reverse-non-term-transitions (kernel-index k) (non-term-index s)))
			k))))))
  
  (define (make-lr0-table auto-hash states syms def)
    (let ((t (make-array2d states syms def)))
      (hash-table-map auto-hash
                      (lambda (k v)
                        (array2d-set! t 
                                      (kernel-index (trans-key-st k))
                                      (gram-sym-index (trans-key-gs k))
                                      v)))
      t))
  
  (define (reverse-hash hash)
    (let ((reverse-hash (make-hash-table 'equal))
	  (hash-table-add!
	   (lambda (ht k v)
	     (hash-table-put! ht k (cons v (hash-table-get ht k (lambda () null)))))))
      (hash-table-for-each hash
			   (lambda (k v)
			     (hash-table-add! reverse-hash 
					      (make-trans-key v (trans-key-gs k))
					      (trans-key-st k))))
      reverse-hash))

			
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
  

  ;; The kernels in the automaton are represented cannonically.
  ;; That is (equal? a b) <=> (eq? a b)
  (define (kernel->string k)
    (apply string-append 
	   `("{" ,@(map (lambda (i) (string-append (item->string i) ", ")) 
			(kernel-items k)) 
	     "}")))


  (define (add-lr0-transition! ttable nttable key value)
    (hash-table-put!
     (if (term? (trans-key-gs key))
         ttable
         nttable)
     key
     value))
		      
  
  ;; build-LR0-automaton: grammar -> LR0-automaton
  ;; Constructs the kernels of the sets of LR(0) items of g
  (define (build-lr0-automaton grammar)
;    (printf "LR(0) automaton:~n")
    (letrec (
	     (terms (list->vector (send grammar get-terms)))
	     (non-terms (list->vector (send grammar get-non-terms)))
	     (num-non-terms (send grammar get-num-non-terms))
	     (num-gram-syms (+ num-non-terms (send grammar get-num-terms)))
	     (epsilons (make-hash-table 'equal))
	     
	     ;; first-non-term: non-term -> non-term list
	     ;; given a non-terminal symbol C, return those non-terminal 
	     ;; symbols A s.t. C -> An for some string of terminals and
	     ;; non-terminals n where -> means a rightmost derivation in many 
	     ;; steps.  Assumes that each non-term can be reduced to a string 
	     ;; of terms.
	     (first-non-term 
	      (digraph (send grammar get-non-terms)
		       (lambda (nt)
			 (filter non-term?
				 (map (lambda (prod)
					(sym-at-dot (make-item prod 0)))
				      (send grammar get-prods-for-non-term nt))))
		       (lambda (nt) (list nt))
		       (union non-term<?)
		       (lambda () null)))
             
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
						(send grammar 
                                                      get-prods-for-non-term
                                                      non-term)))
					 (first-non-term next-gsym)))
			     (LR0-closure (cdr i)))))
		     (else
		      (cons (car i) (LR0-closure (cdr i))))))))))


	     ;; maps trans-keys to kernels
	     (automaton-term (make-hash-table 'equal))
             (automaton-non-term (make-hash-table 'equal))
	     
	     ;; keeps the kernels we have seen, so we can have a unique
	     ;; list for each kernel
	     (kernels (make-hash-table 'equal))

	     (counter 0)
	     
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
			(add-lr0-transition! automaton-term automaton-non-term
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
			
             (starts 
              (map (lambda (init-prod) (list (make-item init-prod 0)))
                   (send grammar get-init-prods)))
	     (startk
              (map (lambda (start)
                     (let ((k (make-kernel start counter)))
                       (hash-table-put! kernels start k)
                       (set! counter (add1 counter))
                       k))
                   starts))
	     (new-kernels (make-queue)))
      
      (let loop ((old-kernels startk)
		 (seen-kernels null))
	(cond
	 ((and (empty-queue? new-kernels) (null? old-kernels))
	  (make-object lr0% 
		       automaton-term
		       automaton-non-term		       
		       (list->vector (reverse! seen-kernels))
		       epsilons
		       (vector-length terms)
		       num-non-terms))
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