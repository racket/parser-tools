(module util mzscheme
  (require (lib "list.ss"))
  
  (provide (all-defined-except split-acc))
  
  (define-struct lex-abbrev (abbrev))
  
  (define-syntax test-block
    (syntax-rules ()
      ((_ defs (code right-ans) ...)
       (let* defs
         (let ((real-ans code))
           (unless (equal? real-ans right-ans)
             (printf "Test failed: ~e gave ~e.  Expected ~e~n"
                     'code real-ans 'right-ans))) ...))))

  #;(define-syntax test-block
    (syntax-rules ()
      ((_ x ...) (void))))
  
  
  ;; A cache is (X ( -> Y) -> Y)
  ;; make-cache : -> cache
  ;; table map Xs to Ys.  If key is mapped, its value is returned.
  ;; Otherwise, build is invoked and its result is placed in the table and
  ;; returned.
  ;; Xs are compared with equal?
  (define (make-cache)
    (let ((table (make-hash-table 'equal)))
      (lambda (key build)
        (hash-table-get table key
                        (lambda ()
                          (let ((new (build)))
                            (hash-table-put! table key new)
                            new))))))
  (test-block ((cache (make-cache)))
              ((cache '(1 2) (lambda () 9)) 9)
              ((cache '(2 1) (lambda () 8)) 8)
              ((cache '(1 2) (lambda () 1)) 9))

  
  ;; make-counter : -> -> nat
  ;; makes a function that returns a higher number by 1, each time
  ;; it is called.
  (define (make-counter)
    (let ((counter 0))
      (lambda ()
        (begin0
          counter
          (set! counter (add1 counter))))))
  (test-block ((c (make-counter))
               (d (make-counter)))
              ((c) 0)
              ((d) 0)
              ((c) 1)
              ((d) 1)
              ((c) 2))
  

  ;; remove-dups : (list-of X) (X -> number) -> (list-of X)
  ;; removes the entries from l that have the same index as a
  ;; previous entry.  l must be grouped by indexes.
  (define (remove-dups l index acc)
    (cond
      ((null? l) (reverse acc))
      ((null? acc) (remove-dups (cdr l) index (cons (car l) acc)))
      ((= (index (car acc)) (index (car l)))
       (remove-dups (cdr l) index acc))
      (else 
       (remove-dups (cdr l) index (cons (car l) acc)))))

  (test-block ()
              ((remove-dups '((1 2) (2 2) (1 3) (1 4) (100 4) (0 5)) cadr null)
               '((1 2) (1 3) (1 4) (0 5)))
              ((remove-dups null error null) null))
  
  
  ;; do-simple-equiv : (list-of X) (X -> nat) -> (list-of X)
  ;; Sorts l according to index and removes the entries with duplicate
  ;; indexes.
  (define (do-simple-equiv l index)
    (let ((ordered (mergesort l (lambda (a b) (< (index a) (index b))))))
      (remove-dups ordered index null)))

  (test-block ()
              ((do-simple-equiv '((2 2) (1 4) (1 2) (100 4) (1 3)  (0 5)) cadr)
               '((2 2) (1 3) (1 4) (0 5)))
              ((do-simple-equiv null error) null))
  

  ;; replace : (list-of X) (X -> bool) (X -> (list-of X)) (list-of X) ->
  ;;           (list-of X)
  ;; If (pred? r) for some r in l, splice (get r) in place of r in the resulting
  ;; list.
  (define (replace l pred? get acc)
    (cond
      ((null? l) acc)
      ((pred? (car l)) (replace (cdr l) pred? get (append (get (car l)) acc)))
      (else (replace (cdr l) pred? get (cons (car l) acc)))))       

  (test-block ()
              ((replace null void (lambda () (list 1)) null) null)
              ((replace '(1 2 3 4 3 5)
                        (lambda (x) (= x 3))
                        (lambda (x) (list 1 2 3))
                        null)
               '(5 1 2 3 4 1 2 3 2 1)))
  
  ;; make-range : int * int -> char list
  ;; creates a list of all chars between i and j.  i <= j
  (define (make-range i j)
    (letrec ((make-range 
              (lambda (i j)
                (cond
                  ((= i j) (list (integer->char i)))
                  (else 
                   (cons (integer->char i) (make-range (add1 i) j)))))))
      (make-range i j)))
  (test-block ()
              ((make-range 97 110) (string->list "abcdefghijklmn"))
              ((make-range 111 111) '(#\o)))
   
  

  ;; merge : (list-of char) (list-of char) -> (list-of char)
  ;; Combines 2 sorted, duplicate-free lists into 1, removing duplicates.
  (define (merge l1 l2)
    (cond
      ((null? l2) l1)
      ((null? l1) l2)
      (else (let ((cl1 (car l1))
                  (cl2 (car l2)))
              (cond
                ((> (char->integer cl1) (char->integer cl2))
                 (cons cl2 (merge l1 (cdr l2))))
                ((< (char->integer cl1) (char->integer cl2))
                 (cons cl1 (merge (cdr l1) l2)))
                (else (merge (cdr l1) l2)))))))
  (test-block ()
              ((merge (string->list "abcd")
                      (string->list "abde"))
               (string->list "abcde"))
              ((merge null null) null)
              ((merge null '(#\1)) '(#\1))
              ((merge '(#\1) null) '(#\1)))
 
  (define (split-acc l1 l2 i l1-i l2-i)
    (cond
      ((null? l1) (values (reverse! i) (reverse! l1-i) (reverse! (append! (reverse l2) l2-i))))
      ((null? l2) (values (reverse! i) (reverse! (append! (reverse l1) l1-i)) (reverse! l2-i)))
      (else (let ((cl1 (car l1))
                  (cl2 (car l2)))
              (cond
                ((> (char->integer cl1) (char->integer cl2))
                 (split-acc l1 (cdr l2) i l1-i (cons cl2 l2-i)))
                ((< (char->integer cl1) (char->integer cl2))
                 (split-acc (cdr l1) l2 i (cons cl1 l1-i) l2-i))
                (else
                 (split-acc (cdr l1) (cdr l2) (cons cl1 i) l1-i l2-i)))))))
      
  ;; split : (list-of char) (list-of char) -> (list-of char) (list-of char) (list-of char)
  ;; Takes sorted, duplicate-free l1 and l2 and returns (l1 intersect l2),
  ;; l1 - (l1 intersect l2) and l2 - (l1 intersect l2)
  (define (split l1 l2)
    (split-acc l1 l2 null null null))
  
  (test-block ()
              ((let-values (((a b c)
                             (split (string->list "abcdghjkl")
                                    (string->list "abdeijmn"))))
                 (list a b c))
               (list (string->list "abdj") (string->list "cghkl") (string->list "eimn")))
              ((let-values (((a b c) (split null null)))
                 (list a b c)) (list null null null))
              ((let-values (((a b c) (split '(#\1) null)))
                 (list a b c)) (list null '(#\1) null))
              ((let-values (((a b c) (split null '(#\1))))
                 (list a b c)) (list null null '(#\1))))
  
  )