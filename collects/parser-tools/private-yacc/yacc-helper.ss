#cs
(module yacc-helper mzscheme

  ;; General helper routines
  
  (provide duplicate-list? remove-duplicates overlap? vector-andmap)
  
  (define (vector-andmap f v)
    (let loop ((i 0))
      (cond
       ((= i (vector-length v)) #t)
       (else (if (f (vector-ref v i))
		 (loop (add1 i))
		 #f)))))

  ;; duplicate-list?: symbol list -> #f | symbol
  ;; returns a symbol that exists twice in l, or false if no such symbol 
  ;; exists
  (define (duplicate-list? l)
    (letrec ((t (make-hash-table))
	     (dl? (lambda (l)
		    (cond
		     ((null? l) #f)
		     ((hash-table-get t (car l) (lambda () #f)) => 
		      (lambda (x) x))
		     (else
		      (hash-table-put! t (car l) (car l))
		      (dl? (cdr l)))))))
      (dl? l)))

  (require (lib "pretty.ss"))
  
  ;; remove-duplicates: syntax-object list -> syntax-object list
  ;; removes the duplicates from the lists
  (define (remove-duplicates sl)
    (let ((t (make-hash-table)))
      (letrec ((x
		(lambda (sl)
		  (cond
		   ((null? sl) sl)
		   ((hash-table-get t (syntax-object->datum (car sl)) (lambda () #f))
		    (x (cdr sl)))
		   (else 
		    (hash-table-put! t (syntax-object->datum (car sl)) #t)
		    (cons (car sl) (x (cdr sl))))))))
	(x sl))))

  ;; overlap?: symbol list * symbol list -> #f | symbol
  ;; Returns an symbol in l1 intersect l2, or #f is no such symbol exists
  (define (overlap? l1 l2)
    (let/ec ret
      (let ((t (make-hash-table)))
	(for-each (lambda (s1)
		    (hash-table-put! t s1 s1))
		  l1)
	(for-each (lambda (s2)
		    (cond 
		     ((hash-table-get t s2 (lambda () #f)) =>
		      (lambda (o) (ret o)))))
		  l2)
	#f)))


)  
  
