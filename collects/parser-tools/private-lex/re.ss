(module re mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           "util.ss")
  
  (provide ->re build-epsilon build-zero build-char-set build-concat
           build-repeat build-or build-and build-neg
           epsilonR? zeroR? char-setR? concatR? repeatR? orR? andR? negR?
           char-setR-chars concatR-re1 concatR-re2 repeatR-re orR-res
           andR-res negR-re
           re-nullable? re-index)
  
  ;; get-index : -> nat
  (define get-index (make-counter))

  ;; An re is either
  ;; - (make-epsilonR bool nat)
  ;; - (make-zeroR bool nat)
  ;; - (make-char-setR bool nat (list-of char))  The list must be sorted
  ;; - (make-concatR bool nat re re)
  ;; - (make-repeatR bool nat re)
  ;; - (make-orR bool nat (list-of re))          Must not directly contain any orRs
  ;; - (make-andR bool nat (list-of re))         Must not directly contain any andRs
  ;; - (make-negR bool nat re)
  ;;
  ;; Every re must have an index field globally different from all
  ;; other re index fields.
  (define-struct re (nullable? index))
  (define-struct (epsilonR re) ())
  (define-struct (zeroR re) ())
  (define-struct (char-setR re) (chars) (make-inspector))
  (define-struct (concatR re) (re1 re2) (make-inspector))
  (define-struct (repeatR re) (re))
  (define-struct (orR re) (res) (make-inspector))
  (define-struct (andR re) (res))
  (define-struct (negR re) (re))

  ;; e : re
  ;; The unique epsilon re
  (define e (make-epsilonR #t (get-index)))

  ;; z : re
  ;; The unique zero re
  (define z (make-zeroR #f (get-index)))


  ;; s-re = char                       match the given character
  ;;      | string                     match its sequence of characters
  ;;      | re                         a precompiled re
  ;;      | (epsilon)                  match the empty string
  ;;      | (* s-re)                   match 0 or more
  ;;      | (+ s-re)                   match 1 or more
  ;;      | (? s-re)                   match 0 or 1
  ;;      | (: s-re ...)               match one of the sub-expressions
  ;;      | (@ s-re ...)               match each sub-expression in succession
  ;;      | (- char char)              match any character between two (inclusive)
  ;;      | (^ char_or_range ...1)     match any character not listed
  ;; (the null concatenation `(@) means epsilon as does "".
  ;;  the null or `(:) means match nothing.  The null carat `(^) means match
  ;;  any character.)

  ;; ->re : s-re cache -> re
  (define (->re exp cache)
    (match exp
      ((? char?) (build-char-set (list exp) cache))
      ((? string?) (->re `(@ ,@(string->list exp)) cache))
      ((? re?) exp)
      (`(epsilon) (build-epsilon))
      (`(* ,r)
        (build-repeat (->re r cache) cache))
      (`(+ ,r)
        (->re `(@ ,r (* ,r)) cache))
      (`(? ,r)
        (let ((c (->re r cache)))
          (if (re-nullable? c)
              c
              (build-or (list e c) cache))))
      (`(: ,r)
        (->re r cache))
      (`(: ,rs ...)
        (build-or (flatten-res (map (lambda (r) (->re r cache)) rs) cache)
                  cache))
      (`(@ ,rs ...)
        (foldr (lambda (x y)
                 (build-concat (->re x cache) y cache))
               e
               rs))
      (`(- ,c1 ,c2)
        (let ((i1 (char->integer (if (string? c1) (string-ref c1 0) c1)))
              (i2 (char->integer (if (string? c2) (string-ref c2 0) c2))))
          (if (<= i1 i2)
              (build-char-set (make-range i1 i2) cache)
              z)))
      (`(^ ,crs ...)
        (let ((cs (->re `(: ,@crs) cache)))
          (cond
            ((zeroR? cs) (build-char-set (make-range 0 255) cache))
            ((char-setR? cs)
             (build-char-set
              (let loop ((bad-chars (map char->integer
                                         (char-setR-chars cs)))
                         (i 0))
                (cond
                  ((> i 255) null)
                  ((and (not (null? bad-chars))
                        (= i (car bad-chars)))
                   (loop (cdr bad-chars) (add1 i)))
                  (else
                   (cons (integer->char i) (loop bad-chars (add1 i))))))
              cache))
            (else z))))))
              

        

  ;; flatten-res: (list-of re) cache -> (list-of re)
  ;; Takes all the char-sets in l and combines them into one element.
  ;; Removes orRs too,
  (define (flatten-res l cache)
    (let loop ((res l)
               (chars null)
               (no-chars null))
      (cond
        ((null? res) 
         (if (null? chars)
             no-chars
             (cons (build-char-set (mergesort chars char<?) cache) no-chars)))
        ((char-setR? (car res))
         (loop (cdr res) (merge (char-setR-chars (car res)) chars) no-chars))
        ((orR? (car res))
         (loop (append (orR-res (car res)) (cdr res)) chars no-chars))
        (else (loop (cdr res) chars (cons (car res) no-chars))))))
    
  ;; build-epsilon : -> re
  (define (build-epsilon) e)
  
  (define (build-zero) z)
    
  ;; build-char-set : (list-of char) cache -> re
  ;; cs must be sorted
  #;(define (build-char-set cs cache)
      (cond
        ((null? cs) z)
        (else
         (make-char-setR #f (get-index) cs))))
  
  (define (build-char-set cs cache)
    (cond
      ((null? cs) z)
      (else
       (cache cs
              (lambda ()
                (make-char-setR #f (get-index) cs))))))
  
  
  
  ;; build-concat : re re cache -> re
  (define (build-concat r1 r2 cache)
      (cond
        ((eq? e r1) r2)
        ((eq? e r2) r1)
        ((or (eq? z r1) (eq? z r2)) z)
        (else
         (let* ((i1 (re-index r1))
                (i2 (re-index r2))
                (key (if (< i1 i2)
                         (cons i1 i2)
                         (cons i2 i1))))
           (cache (cons 'concat key)
                  (lambda ()
                    (make-concatR (and (re-nullable? r1) (re-nullable? r2))
                                  (get-index)
                                  r1 r2)))))))
  
  ;; build-repeat : re cache -> re
  (define (build-repeat r cache)
    (cache (cons 'repeat (re-index r))
           (lambda ()
             (make-repeatR #t (get-index) r))))
  
  
  ;; build-or : (list-of re) cache -> re
  (define (build-or rs cache)
    (let ((rs 
           (filter
            (lambda (x) (not (eq? x z)))
            (do-simple-equiv (replace rs orR? orR-res null) re-index))))
      (cond
        ((null? rs) z)
        ((null? (cdr rs)) (car rs))
        (else
         (cache (cons 'or (map re-index rs))
                (lambda ()
                  (make-orR (ormap re-nullable? rs) (get-index) rs)))))))
  
  ;; build-and : (list-of re) cache -> re
  (define (build-and rs cache)
    (let ((rs (do-simple-equiv (replace rs andR? andR-res null) rs)))
      (cond
        ((ormap (lambda (x) (eq? x z)) rs) z)
        (else
         (cache (cons 'and (map re-index rs))
                (lambda ()
                  (make-andR (andmap re-nullable? rs) (get-index) rs)))))))
      
  ;; build-neg : re cache -> re
  (define (build-neg r cache)
    (cache (cons 'neg (re-index r))
           (lambda ()
             (make-negR (not (re-nullable? r)) (get-index) r))))
  
  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c))
               (rc (->re `(@ ,r1 ,r2) c))
               (rc2 (->re `(@ ,r2 ,r1) c))
               (rr (->re `(* ,rc) c))
               (ro (->re `(: ,rr ,rc ,rr) c))
               (ro2 (->re `(: ,rc ,rr ,z) c))
               (ro3 (->re `(: ,rr ,rc) c)))
              (rc rc2)
              (ro ro2)
              (ro ro3)
              ((->re `(* ,rc) c) rr)
              ((build-char-set null c) z)
              ((->re `(@ ,r1 (epsilon)) c) r1)
              ((->re `(@ (epsilon) ,r1) c) r1)
              ((->re `(@ ,r1 ,z) c) z)
              ((->re `(@ ,z ,r1) c) z)
              ((->re `(@ ,z (epsilon)) c) z)
              ((->re `(@ (epsilon) ,z) c) z)
              ((->re `(:) c) z)
              ((->re `(: ,rr) c) rr)
              ((build-or `(,z ,r1 ,z) c) r1)
              ((build-or (list
                          (build-or (list r1 r2) c)
                          (build-or (list rc rr) c))
                         c)
               (build-or (list r1 r2 rc rr) c))
              ((concatR-re1 rc2) r1)
              ((concatR-re2 rc2) r2)
              ((orR-res ro) (list rc rr))
              ((repeatR-re rr) rc)
              ((re-nullable? r1) #f)
              ((re-nullable? rc) #f)
              ((re-nullable? (->re `(@ ,rr ,rr) c)) #t)
              ((re-nullable? rr) #t)
              ((re-nullable? ro) #t)
              ((re-nullable? (->re `(: ,r1 ,r2) c)) #f))
              
  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c))
               (r3-5 (->re '(- #\3 #\5) c))
               (r4 (build-or `(,r1 ,r2) c))
               (r5 (->re `(: ,r3-5 #\7) c))
               (r6 (->re #\6 c)))
              ((flatten-res null c) null)
              ((char-setR-chars (car (flatten-res `(,r1) c))) '(#\1))
              ((char-setR-chars (car (flatten-res `(,r4) c))) '(#\1 #\2))
              ((char-setR-chars (car (flatten-res `(,r6 ,r5 ,r4 ,r3-5 ,r2 ,r1) c)))
               (string->list "1234567")))
  
  (test-block ((c (make-cache))
               (r (->re #\a c))
               (rr (->re `(@ ,r ,r) c))
               (rrr (->re `(@ ,r ,rr) c))
               (rrr* (->re `(* ,rrr) c)))
              ((char-setR-chars r) '(#\a))
              ((->re "" c) e)
              ((->re "asdf" c) (->re `(@ #\a #\s #\d #\f) c))
              ((->re r c) r)
              ((->re `(epsilon) c) e)
              ((->re `(* ,r) c) (build-repeat r c))
              ((->re `(+ ,r) c) (build-concat r (build-repeat r c) c))
              ((->re `(? ,r) c) (build-or (list e r) c))
              ((->re `(? ,rrr*) c) rrr*)
              ((->re `(: (: (- #\a #\c) (^ (- #\000 #\110) (- #\112 #\377)))
                         (: (* #\2))) c)
               (build-or (list (build-char-set (list #\111 #\a #\b #\c) c)
                               (build-repeat (build-char-set '(#\2) c) c))
                         c))
              ((->re `(: ,rr ,rrr) c) (build-or (list rr rrr) c))
              ((->re `(: ,r) c) r)
              ((->re `(:) c) z)
              ((->re `(@) c) e)
              ((->re `(@ ,rrr*) c) rrr*)
              (rr (build-concat r r c))
              ((->re `(@ ,r ,rr ,rrr) c)
               (build-concat r (build-concat rr rrr c) c))
              ((char-setR-chars (->re `(- #\1 #\1) c)) '(#\1))
              ((char-setR-chars (->re `(- #\1 #\9) c)) (string->list "123456789"))
              ((char-setR-chars (->re `(- "1" "1") c)) '(#\1))
              ((char-setR-chars (->re `(- "1" "9") c)) (string->list "123456789"))
              ((->re `(- "9" "1") c) z)
              ((char-setR-chars (->re `(^) c))
               (char-setR-chars (->re `(- #\000 #\377) c)))
              ((char-setR-chars (->re `(^ #\001 (- #\002 #\377)) c)) `(#\000))
              )
  
  )