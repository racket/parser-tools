#cs
(module table mzscheme

  ;; Routine to build the LALR table
  
  (require "grammar.ss"
	   "lr0.ss"
	   "array2d.ss"
	   "lalr.ss"
	   "parser-actions.ss"
	   (lib "list.ss")
	   (lib "class.ss"))

  (provide build-table)

  
  (define (bit-vector-for-each f bv)
    (letrec ((for-each
              (lambda (bv number)
                (cond
                  ((= 0 bv) (void))
                  ((= 1 (bitwise-and 1 bv))
                   (f number)
                   (for-each (arithmetic-shift bv -1) (add1 number)))
                  (else (for-each (arithmetic-shift bv -1) (add1 number)))))))
      (for-each bv 0)))
                   
  
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
  (define (display-parser a table terms non-terms prods port)
    (let* ((num-terms (vector-length terms))
	   (num-non-terms (vector-length non-terms))
	   (SR-conflicts 0)
	   (RR-conflicts 0))
      (for-each
       (lambda (prod)
         (fprintf port 
                  "~a\t~a\t=\t~a~n" 
                  (prod-index prod)
                  (gram-sym-symbol (prod-lhs prod))
                  (map gram-sym-symbol (vector->list (prod-rhs prod)))))
       prods)
      (send a for-each-state
       (lambda (state)
         (fprintf port "State ~a~n" (kernel-index state))
         (for-each (lambda (item)
                     (fprintf port "\t~a~n" (item->string item)))
                   (kernel-items state))
         (newline port)
         (let loop ((j 0))
           (if (< j num-terms)
               (begin
                 (let ((act (array2d-ref 
                             table 
                             (kernel-index state)
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
                 (let ((s (array2d-ref table (kernel-index state) j)))
                   (if s
                       (print-entry
                        (gram-sym-symbol (vector-ref non-terms j))
                        s
                        port)))
                 (loop (add1 j)))))
         
         (newline port)))

      (if (> SR-conflicts 0)
	  (fprintf port "~a shift/reduce conflicts~n" SR-conflicts))
      (if (> RR-conflicts 0)
	  (fprintf port "~a reduce/reduce conflicts~n" RR-conflicts))))

  (define (resolve-conflicts a table num-terms num-non-terms suppress)
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
                     ((and (reduce? (car rest))
                           (< (reduce-prod-num (car rest))
                              (reduce-prod-num current-guess)))
		      (loop (car rest) (cdr rest)))
                     ((accept? (car rest))
                      (fprintf (current-error-port) 
                               "accept/reduce or accept/shift conflicts.  Check the grammar for useless cycles of productions~n")
                      (loop current-guess (cdr rest)))
                     (else (loop current-guess (cdr rest))))))
		 (else entry)))))
      (send a for-each-state
       (lambda (state)
         (let loop ((term 0))
           (if (< term num-terms)
               (begin
                 (array2d-set! table (kernel-index state) (+ num-non-terms term)
                               (get-action
                                (array2d-ref table 
                                             (kernel-index state) 
                                             (+ num-non-terms term))))
                 (loop (add1 term)))))))
      (if (not suppress)
          (begin
            (if (> SR-conflicts 0)
                (fprintf (current-error-port) 
                         "~a shift/reduce conflicts~n" 
                         SR-conflicts))
            (if (> RR-conflicts 0)
                (fprintf (current-error-port)
                         "~a reduce/reduce conflicts~n"
                         RR-conflicts))))))
    


  (define (resolve-prec-conflicts a table get-term get-prod
				  num-terms num-non-terms)
    (send a for-each-state
     (lambda (state)
       (let loop ((term 0))
         (if (< term num-terms)
             (begin
               (let ((action (array2d-ref table 
                                          (kernel-index state)
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
                            (kernel-index state)
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
               (loop (add1 term))))))))

  ;; In the result table the first index is the state and the second is the 
  ;; term/non-term index (with the non-terms coming first)
  ;; buile-table: grammar * string -> action array2d
  (define (build-table g file suppress)
    (let* ((a (build-lr0-automaton g))
           (num-terms (send g get-num-terms))
           (num-non-terms (send g get-num-non-terms))
           (get-term (list->vector (send g get-terms)))
           (get-non-term (list->vector (send g get-non-terms)))
           (get-prod (list->vector (send g get-prods)))
           (end-term-indexes 
            (map
             (lambda (term)
               (+ num-non-terms (gram-sym-index term)))
             (send g get-end-terms)))
           (num-gram-syms (+ num-terms num-non-terms))
           (table (make-array2d (send a get-num-states) num-gram-syms #f))
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
      
      (send a for-each-state
            (lambda (state)
              (let loop ((i 0))
                (if (< i num-gram-syms)
                    (begin
                      (let* ((s (if (< i num-non-terms)
                                    (vector-ref get-non-term i)
                                    (vector-ref get-term (- i num-non-terms))))
                             (goto
                              (send a run-automaton state s)))
                        (if goto
                            (array2d-set! table 
                                          (kernel-index state) 
                                          i
                                          (cond
                                            ((< i num-non-terms)
                                             (kernel-index goto))
                                            ((member i end-term-indexes)
                                             (make-accept))
                                            (else
                                             (make-shift 
                                              (kernel-index goto)))))))
                      (loop (add1 i)))))
              (for-each
               (lambda (item)
                 (let ((item-prod (item-prod item)))
                   (bit-vector-for-each 
                    (lambda (term-index)
                      (array2d-add! table 
                                    (kernel-index state)
                                    (+ num-non-terms term-index)
                                    (cond
                                      ((not (start-item? item))
                                       (make-reduce
                                        (prod-index item-prod)
                                        (gram-sym-index (prod-lhs item-prod))
                                        (vector-length (prod-rhs item-prod)))))))
                    (get-lookahead state item-prod))))
               
               (append (hash-table-get (send a get-epsilon-trans) state (lambda () null))
                       (filter (lambda (item)
                                 (not (move-dot-right item)))
                               (kernel-items state))))))
      
      
      (resolve-prec-conflicts a table get-term get-prod num-terms
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
                (display-parser a table get-term get-non-term (send g get-prods)
                                port)))))
      
      (resolve-conflicts a table num-terms num-non-terms suppress)
      
      table))
  
  )


