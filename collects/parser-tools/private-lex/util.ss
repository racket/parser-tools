(module util mzscheme
  (require (lib "list.ss"))
  
  (provide (all-defined-except split-acc complement-acc))
  
  (define-struct lex-abbrev (abbrev))
  
  #;(define-syntax test-block
    (syntax-rules ()
      ((_ defs (code right-ans) ...)
       (let* defs
         (let ((real-ans code))
           (unless (equal? real-ans right-ans)
             (printf "Test failed: ~e gave ~e.  Expected ~e~n"
                     'code real-ans 'right-ans))) ...))))

  (define-syntax test-block
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
  
  
  ;; A char-set is (list-of (cons nat nat))
  ;; Each cons represents a range of characters, and the entire
  ;; set is the union of the ranges.  The ranges must be disjoint and
  ;; increasing.  Further, adjacent ranges must have at least
  ;; one number between them.
  
  
  (define (nat? x)
    (and (integer? x) (exact? x) (>= x 0)))
  
  ;; char-set? : X -> bool
  (define (char-set? x)
    (let loop ((set x)
               (current-num -2))
      (or
       (null? set)
       (and (pair? set)
            (pair? (car set))
            (nat? (caar set))
            (nat? (cdar set))
            (< (add1 current-num) (caar set))
            (<= (caar set) (cdar set))
            (loop (cdr set) (cdar set))))))
  (test-block ()
              ((char-set? '((0 . 4) (7 . 9))) #t)
              ((char-set? '((-1 . 4))) #f)
              ((char-set? '((11 . 10))) #f)
              ((char-set? '((0 . 10) (8 . 12))) #f)
              ((char-set? '((10 . 20) (1 . 2))) #f)
              ((char-set? '((1 . 1))) #t)
              ((char-set? '((1 . 1) (2 . 3))) #f)
              ((char-set? '((1 . 1) (3 . 3))) #t)
              ((char-set? null) #t))
               

  
  ;; make-range : int * int -> char-set
  ;; creates a set of chars between i and j.  i <= j
  (define (make-range i j)
    (list (cons i j)))
  (test-block ()
              ((make-range 97 110) '((97 . 110)))
              ((make-range 111 111) '((111 . 111))))
   
  
  ;; sub-range? : (cons int int) (cons int int) -> bool
  ;; true iff the interval [(car r1), (cdr r1)] is a subset of
  ;; [(car r2), (cdr r2)]
  (define (sub-range? r1 r2)
    (and (>= (car r1) (car r2))
         (<= (cdr r1) (cdr r2))))
    
  ;; overlap? : (cons int int) (cons int int) -> bool
  ;; true iff the intervals [(car r1), (cdr r1)] and [(car r2), (cdr r2)]
  ;; have non-empty intersections and (car r1) >= (car r2)
  (define (overlap? r1 r2)
    (and (>= (car r1) (car r2))
         (>= (cdr r1) (cdr r2))
         (<= (car r1) (cdr r2))))
    
  
  ;; merge : char-set char-set -> char-set
  ;; unions 2 char-sets
  (define (merge s1 s2)
    (cond
      ((null? s2) s1)
      ((null? s1) s2)
      (else 
       (let ((r1 (car s1))
             (r2 (car s2)))
         (cond
           ((sub-range? r1 r2) (merge (cdr s1) s2))
           ((sub-range? r2 r1) (merge s1 (cdr s2)))
           ((or (overlap? r1 r2) (= (car r1) (add1 (cdr r2))))
            (merge (cons (cons (car r2) (cdr r1)) (cdr s1)) (cdr s2)))
           ((or (overlap? r2 r1) (= (car r2) (add1 (cdr r1))))
            (merge (cdr s1) (cons (cons (car r1) (cdr r2)) (cdr s2))))
           ((< (car r1) (car r2))
            (cons r1 (merge (cdr s1) s2)))
           (else
            (cons r2 (merge s1 (cdr s2)))))))))
  (test-block ()
              ((merge null null) null)
              ((merge null '((1 . 10))) '((1 . 10)))
              ((merge '((1 . 10)) null) '((1 . 10)))
              ;; r1 in r2
              ((merge '((5 . 10)) '((5 . 10))) '((5 . 10)))
              ((merge '((6 . 9)) '((5 . 10))) '((5 . 10)))
              ((merge '((7 . 7)) '((5 . 10))) '((5 . 10)))
              ;; r2 in r1
              ((merge '((5 . 10)) '((5 . 10))) '((5 . 10)))
              ((merge '((5 . 10)) '((6 . 9))) '((5 . 10)))
              ((merge '((5 . 10)) '((7 . 7))) '((5 . 10)))
              ;; r2 and r1 are disjoint
              ((merge '((5 . 10)) '((12 . 14))) '((5 . 10) (12 . 14)))
              ((merge '((12 . 14)) '((5 . 10))) '((5 . 10) (12 . 14)))
              ;; r1 and r1 are adjacent
              ((merge '((5 . 10)) '((11 . 13))) '((5 . 13)))
              ((merge '((11 . 13)) '((5 . 10))) '((5 . 13)))
              ;; r1 and r2 overlap
              ((merge '((5 . 10)) '((7 . 14))) '((5 . 14)))
              ((merge '((7 . 14)) '((5 . 10))) '((5 . 14)))
              ((merge '((5 . 10)) '((10 . 14))) '((5 . 14)))
              ((merge '((7 . 10)) '((5 . 7))) '((5 . 10)))
              ;; with lists
              ((merge '((1 . 1) (3 . 3) (5 . 10) (100 . 200))
                      '((2 . 2) (10 . 12) (300 . 300)))
               '((1 . 3) (5 . 12) (100 . 200) (300 . 300)))
              ((merge '((1 . 1) (3 . 3) (5 . 5) (8 . 8) (10 . 10) (12 . 12))
                      '((2 . 2) (4 . 4) (6 . 7) (9 . 9) (11 . 11)))
               '((1 . 12)))
              ((merge '((2 . 2) (4 . 4) (6 . 7) (9 . 9) (11 . 11))
                      '((1 . 1) (3 . 3) (5 . 5) (8 . 8) (10 . 10) (12 . 12)))
               '((1 . 12))))
                      
  
  ;; split-sub-range : (cons int int) (cons int int) -> char-set
  ;; (subrange? r1 r2) must hold.
  ;; returns [(car r2), (cdr r2)] - ([(car r1), (cdr r1)] intersect [(car r2), (cdr r2)]).
  (define (split-sub-range r1 r2)
    (let ((r1-car (car r1))
          (r1-cdr (cdr r1))
          (r2-car (car r2))
          (r2-cdr (cdr r2)))
      (cond
        ((and (= r1-car r2-car) (= r1-cdr r2-cdr)) null)
        ((= r1-car r2-car) (list (cons (add1 r1-cdr) r2-cdr)))
        ((= r1-cdr r2-cdr) (list (cons r2-car (sub1 r1-car))))
        (else
         (list (cons r2-car (sub1 r1-car)) (cons (add1 r1-cdr) r2-cdr))))))

  (test-block ()
              ((split-sub-range '(1 . 10) '(1 . 10)) '())
              ((split-sub-range '(1 . 5) '(1 . 10)) '((6 . 10)))
              ((split-sub-range '(2 . 10) '(1 . 10)) '((1 . 1)))
              ((split-sub-range '(2 . 5) '(1 . 10)) '((1 . 1) (6 . 10))))
  
  
  (define (split-acc s1 s2 i s1-i s2-i)
    (cond
      ((null? s1) (values (reverse! i) (reverse! s1-i) (reverse! (append! (reverse s2) s2-i))))
      ((null? s2) (values (reverse! i) (reverse! (append! (reverse s1) s1-i)) (reverse! s2-i)))
      (else
       (let ((r1 (car s1))
             (r2 (car s2)))
         (cond
           ((sub-range? r1 r2)
            (split-acc (cdr s1) (append (split-sub-range r1 r2) (cdr s2))
                       (cons r1 i) s1-i s2-i))
           ((sub-range? r2 r1)
            (split-acc (append (split-sub-range r2 r1) (cdr s1)) (cdr s2)
                       (cons r2 i) s1-i s2-i))
           ((overlap? r1 r2)
            (split-acc (cons (cons (add1 (cdr r2)) (cdr r1)) (cdr s1))
                       (cdr s2)
                       (cons (cons (car r1) (cdr r2)) i)
                       s1-i 
                       (cons (cons (car r2) (sub1 (car r1))) s2-i)))
           ((overlap? r2 r1)
            (split-acc (cdr s1)
                       (cons (cons (add1 (cdr r1)) (cdr r2)) (cdr s2))
                       (cons (cons (car r2) (cdr r1)) i)
                       (cons (cons (car r1) (sub1 (car r2)))s1-i )
                       s2-i))
           ((< (car r1) (car r2))
            (split-acc (cdr s1) s2 i (cons r1 s1-i) s2-i))
           (else
            (split-acc s1 (cdr s2) i s1-i (cons r2 s2-i))))))))
  
  ;; split : char-set -> char-set char-set char-set
  ;; returns (l1 intersect l2), l1 - (l1 intersect l2) and l2 - (l1 intersect l2)
  (define (split s1 s2)
    (split-acc s1 s2 null null null))
  
  (test-block ((s (lambda (s1 s2)
                    (call-with-values (lambda () (split s1 s2)) list))))
              ((s null null) '(() () ()))
              ((s '((1 . 10)) null) '(() ((1 . 10)) ()))
              ((s null '((1 . 10))) '(() () ((1 . 10))))
              ((s '((1 . 10)) null) '(() ((1 . 10)) ()))
              ((s '((1 . 10)) '((1 . 10))) '(((1 . 10)) () ()))
              ((s '((1 . 10)) '((2 . 5))) '(((2 . 5)) ((1 . 1) (6 . 10)) ()))
              ((s '((2 . 5)) '((1 . 10))) '(((2 . 5)) () ((1 . 1) (6 . 10))))
              ((s '((2 . 5)) '((5 . 10))) '(((5 . 5)) ((2 . 4)) ((6 . 10))))
              ((s '((5 . 10)) '((2 . 5))) '(((5 . 5)) ((6 . 10)) ((2 . 4))))
              ((s '((2 . 10)) '((5 . 14))) '(((5 . 10)) ((2 . 4)) ((11 . 14))))
              ((s '((5 . 14)) '((2 . 10))) '(((5 . 10)) ((11 . 14)) ((2 . 4))))
              ((s '((10 . 20)) '((30 . 50))) '(() ((10 . 20)) ((30 . 50))))
              ((s '((100 . 200)) '((30 . 50))) '(() ((100 . 200)) ((30 . 50))))
              ((s '((1 . 5) (7 . 9) (100 . 200) (500 . 600) (600 . 700))
                  '((2 . 8) (50 . 60) (101 . 104) (105 . 220)))
               '(((2 . 5) (7 . 8) (101 . 104) (105 . 200))
                 ((1 . 1) (9 . 9) (100 . 100) (500 . 600) (600 . 700))
                 ((6 . 6) (50 . 60) (201 . 220))))
              ((s '((2 . 8) (50 . 60) (101 . 104) (105 . 220))
                  '((1 . 5) (7 . 9) (100 . 200) (500 . 600) (600 . 700)))
               '(((2 . 5) (7 . 8) (101 . 104) (105 . 200))
                 ((6 . 6) (50 . 60) (201 . 220))
                 ((1 . 1) (9 . 9) (100 . 100) (500 . 600) (600 . 700))))
              )
  
  ;; complement-acc : char-set nat nat -> char-set
  ;; As complement.  The current-nat accumulator keeps track of where the
  ;; next range in the complement should start.
  (define (complement-acc s current-nat max)
    (cond
      ((null? s) (if (<= current-nat max)
                     (list (cons current-nat max))
                     null))
      (else
       (let ((s-car (car s)))
         (cond
           ((< current-nat (car s-car))
            (cons (cons current-nat (sub1 (car s-car)))
                  (complement-acc (cdr s) (add1 (cdr s-car)) max)))
           ((<= current-nat (cdr s-car))
            (complement-acc (cdr s) (add1 (cdr s-car)) max))
           (else
            (complement-acc (cdr s) current-nat max)))))))
  
    
  ;; complement : char-set nat -> char-set
  ;; A set of all the nats not in s, up to and including max.
  ;; (cdr (last-pair s)) <= max
  (define (complement s max)
    (complement-acc s 0 max))
  (test-block ()
              ((complement null 255) '((0 . 255)))
              ((complement '((1 . 5) (7 . 7) (10 . 200)) 255)
               '((0 . 0) (6 . 6) (8 . 9) (201 . 255)))
              ((complement '((0 . 254)) 255) '((255 . 255)))
              ((complement '((1 . 255)) 255) '((0 . 0)))
              ((complement '((0 . 255)) 255) null))
  
  ;; char-in-set? : nat char-set -> bool
  (define (char-in-set? c cs)
    (and
      (pair? cs)
      (or (<= (caar cs) c (cdar cs))
          (char-in-set? c (cdr cs)))))
  (test-block ()
              ((char-in-set? 1 null) #f)
              ((char-in-set? 19 '((1 . 18) (20 . 21))) #f)
              ((char-in-set? 19 '((1 . 2) (19 . 19) (20 . 21))) #t))
               
  (define get-a-char car)
  
  (define (char-set->string cs)
    (cond
      ((null? cs) "")
      (else
       (string-append (format "~a(~a)-~a(~a) "
                              (caar cs) (integer->char (caar cs))
                              (cdar cs) (integer->char (cdar cs)))
                      (char-set->string (cdr cs))))))
  
  (define (char-for-each-acc f start stop cs)
    (cond
      ((and (> start stop) (null? cs)) (void))
      ((> start stop)
       (char-for-each-acc f (caar cs) (cdar cs) (cdr cs)))
      (else
       (f start)
       (char-for-each-acc f (add1 start) stop cs))))
  
  (define (char-set-for-each f cs)
    (cond
      ((null? cs) (void))
      (else
       (char-for-each-acc f (caar cs) (cdar cs) (cdr cs)))))
  
    )
