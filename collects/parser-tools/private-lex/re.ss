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
  ;; - (make-char-setR bool nat char-set)
  ;; - (make-concatR bool nat re re)
  ;; - (make-repeatR bool nat re)
  ;; - (make-orR bool nat (list-of re))          Must not directly contain any orRs
  ;; - (make-andR bool nat (list-of re))         Must not directly contain any andRs
  ;; - (make-negR bool nat re)
  ;;
  ;; Every re must have an index field globally different from all
  ;; other re index fields.
  (define-struct re (nullable? index) (make-inspector))
  (define-struct (epsilonR re) () (make-inspector))
  (define-struct (zeroR re) () (make-inspector))
  (define-struct (char-setR re) (chars) (make-inspector))
  (define-struct (concatR re) (re1 re2) (make-inspector))
  (define-struct (repeatR re) (re) (make-inspector))
  (define-struct (orR re) (res) (make-inspector))
  (define-struct (andR re) (res) (make-inspector))
  (define-struct (negR re) (re) (make-inspector))

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
  ;;      | (& s-re ...)               match iff all sub-expressions match
  ;;      | (~ s-re)                   match iff s-re doesn't match
  ;;      | (@ s-re ...)               match each sub-expression in succession
  ;;      | (- char char)              match any character between two (inclusive)
  ;;      | (^ char_or_range ...)     match any character not listed
  ;; (The null concatenation `(@) means epsilon as does "".
  ;;  The null or `(:) means match nothing.  The null carat `(^) means match
  ;;  any character.  The null intersection `(&) means match string.)

  ;; ->re : s-re cache -> re
  (define (->re exp cache)
    (match exp
      ((? char?) (build-char-set (make-range (char->integer exp) (char->integer exp)) cache))
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
      (`(: ,rs ...)
        (build-or (flatten-res (map (lambda (r) (->re r cache)) rs)
                               orR? orR-res merge cache)
                  cache))
      (`(& ,rs ...)
        (build-and (flatten-res (map (lambda (r) (->re r cache)) rs)
                                andR? andR-res (lambda (a b)
                                                 (let-values (((i _ __) (split a b))) i))
                                cache)
                   cache))
      (`(~ ,r)
        (build-neg (->re r cache) cache))
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
             (build-char-set (complement (char-setR-chars cs) 255) cache))
            (else z))))))
              

        

  ;; flatten-res: (list-of re) (re -> bool) (re -> (list-of re))
  ;;              (char-set char-set -> char-set) cache -> (list-of re)
  ;; Takes all the char-sets in l and combines them into one char-set using the combine function.
  ;; Flattens out the values of type?.  get-res only needs to function on things type? returns
  ;; true for.
  (define (flatten-res l type? get-res combine cache)
    (let loop ((res l)
               ;; chars : (union #f char-set)
               (chars #f)
               (no-chars null))
      (cond
        ((null? res) 
         (if chars
             (cons (build-char-set chars cache) no-chars)
             no-chars))
        ((char-setR? (car res))
         (if chars
             (loop (cdr res) (combine (char-setR-chars (car res)) chars) no-chars)
             (loop (cdr res) (char-setR-chars (car res)) no-chars)))
        ((type? (car res))
         (loop (append (get-res (car res)) (cdr res)) chars no-chars))
        (else (loop (cdr res) chars (cons (car res) no-chars))))))
    
  ;; build-epsilon : -> re
  (define (build-epsilon) e)
  
  (define (build-zero) z)
    
  ;; build-char-set : char-set cache -> re
  #;(define (build-char-set cs cache)
      (cond
        ((null? cs) z)
        (else
         (make-char-setR #f (get-index) cs))))
  

  ;; build-char-set : char-set cache -> re
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
           (cache (cons 'concat (cons (re-index r1) (re-index r2)))
                  (lambda ()
                    (make-concatR (and (re-nullable? r1) (re-nullable? r2))
                                  (get-index)
                                  r1 r2))))))
  
  ;; build-repeat : re cache -> re
  (define (build-repeat r cache)
    (cond
      ((eq? z r) e)
      ((repeatR? r) r)
      (else
       (cache (cons 'repeat (re-index r))
              (lambda ()
                (make-repeatR #t (get-index) r))))))
  
  
  ;; build-or : (list-of re) cache -> re
  (define (build-or rs cache)
    (let ((rs 
           (filter
            (lambda (x) (not (eq? x z)))
            (do-simple-equiv (replace rs orR? orR-res null) re-index))))
      (cond
        ((null? rs) z)
        ((null? (cdr rs)) (car rs))
        ((memq (build-neg z cache) rs) (build-neg z cache))
        (else
         (cache (cons 'or (map re-index rs))
                (lambda ()
                  (make-orR (ormap re-nullable? rs) (get-index) rs)))))))
  
  ;; build-and : (list-of re) cache -> re
  (define (build-and rs cache)
    (let ((rs (do-simple-equiv (replace rs andR? andR-res null) re-index)))
      (cond
        ((null? rs) (build-neg z cache))
        ((null? (cdr rs)) (car rs))
        ((memq z rs) z)
        (else
         (cache (cons 'and (map re-index rs))
                (lambda ()
                  (make-andR (andmap re-nullable? rs) (get-index) rs)))))))
      
  ;; build-neg : re cache -> re
  (define (build-neg r cache)
    (cond
      ((negR? r) (negR-re r))
      (else
       (cache (cons 'neg (re-index r))
              (lambda ()
                (make-negR (not (re-nullable? r)) (get-index) r))))))
  
  ;; Tests for the build-functions
  (test-block ((c (make-cache))
               (r1 (build-char-set (make-range (char->integer #\1) (char->integer #\1)) c))
               (r2 (build-char-set (make-range (char->integer #\2) (char->integer #\2)) c))
               (r3 (build-char-set (make-range (char->integer #\3) (char->integer #\3)) c))
               (rc (build-concat r1 r2 c))
               (rc2 (build-concat r2 r1 c))
               (rr (build-repeat rc c))
               (ro (build-or `(,rr ,rc ,rr) c))
               (ro2 (build-or `(,rc ,rr ,z) c))
               (ro3 (build-or `(,rr ,rc) c))
               (ro4 (build-or `(,(build-or `(,r1 ,r2) c)
                                 ,(build-or `(,r2 ,r3) c)) c))
               (ra (build-and `(,rr ,rc ,rr) c))
               (ra2 (build-and `(,rc ,rr) c))
               (ra3 (build-and `(,rr ,rc) c))
               (ra4 (build-and `(,(build-and `(,r3 ,r2) c)
                                  ,(build-and `(,r2 ,r1) c)) c))
               (rn (build-neg z c))
               (rn2 (build-neg r1 c)))
               
              ((char-setR-chars r1) (make-range (char->integer #\1) (char->integer #\1)))
              ((char-setR-chars r2) (make-range (char->integer #\2) (char->integer #\2)))
              ((char-setR-chars r3) (make-range (char->integer #\3) (char->integer #\3)))
              ((build-char-set null c) z)
              ((build-concat r1 e c) r1)
              ((build-concat e r1 c) r1)
              ((build-concat r1 z c) z)
              ((build-concat z r1 c) z)
              ((build-concat r1 r2 c) rc)
              ((concatR-re1 rc) r1)
              ((concatR-re2 rc) r2)
              ((concatR-re1 rc2) r2)
              ((concatR-re2 rc2) r1)
              (ro ro2)
              (ro ro3)
              (ro4 (build-or `(,r1 ,r2 ,r3) c))
              ((orR-res ro) (list rc rr))
              ((orR-res ro4) (list r1 r2 r3))
              ((build-or null c) z)
              ((build-or `(,r1 ,z) c) r1)
              ((build-repeat rc c) rr)
              ((build-repeat z c) e)
              ((build-repeat (build-repeat rc c) c) (build-repeat rc c))
              ((repeatR-re rr) rc)
              (ra ra2)
              (ra ra3)
              (ra4 (build-and `(,r1 ,r2 ,r3) c))
              ((andR-res ra) (list rc rr))
              ((andR-res ra4) (list r1 r2 r3))
              ((build-and null c) (build-neg z c))
              ((build-and `(,r1 ,z) c) z)
              ((build-and `(,r1) c) r1)
              ((build-neg r1 c) (build-neg r1 c))
              ((build-neg (build-neg r1 c) c) r1)
              ((negR-re (build-neg r2 c)) r2)
              ((re-nullable? r1) #f)
              ((re-nullable? rc) #f)
              ((re-nullable? (build-concat rr rr c)) #t)
              ((re-nullable? rr) #t)
              ((re-nullable? ro) #t)
              ((re-nullable? (build-or `(,r1 ,r2) c)) #f)
              ((re-nullable? (build-and `(,r1 ,e) c)) #f)
              ((re-nullable? (build-and `(,rr ,e) c)) #t)
              ((re-nullable? (build-neg r1 c)) #t)
              ((re-nullable? (build-neg rr c)) #f))
              
  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c))
               (r3-5 (->re '(- #\3 #\5) c))
               (r4 (build-or `(,r1 ,r2) c))
               (r5 (->re `(: ,r3-5 #\7) c))
               (r6 (->re #\6 c)))
              ((flatten-res null orR? orR-res merge c) null)
              ((char-setR-chars (car (flatten-res `(,r1) orR? orR-res merge c)))
               (make-range (char->integer #\1) (char->integer #\1)))               
              ((char-setR-chars (car (flatten-res `(,r4) orR? orR-res merge c)))
               (make-range (char->integer #\1) (char->integer #\2)))
              ((char-setR-chars (car (flatten-res `(,r6 ,r5 ,r4 ,r3-5 ,r2 ,r1) orR? orR-res merge c)))
               (make-range (char->integer #\1) (char->integer #\7)))
              ((flatten-res `(,r1 ,r2) andR? andR-res (lambda (x y)
                                                        (let-values (((i _ __)
                                                                      (split x y)))
                                                          i))
                            c)
               (list z)))
  
  ;; ->re
  (test-block ((c (make-cache))
               (r (->re #\a c))
               (rr (->re `(@ ,r ,r) c))
               (rrr (->re `(@ ,r ,rr) c))
               (rrr* (->re `(* ,rrr) c)))
              ((char-setR-chars r) (make-range (char->integer #\a) (char->integer #\a)))
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
               (build-or (list (build-char-set (append (make-range 73 73)
                                                       (make-range 97 99))
                                               c)
                               (build-repeat (build-char-set (make-range 50 50) c) c))
                         c))
              ((->re `(: ,rr ,rrr) c) (build-or (list rr rrr) c))
              ((->re `(: ,r) c) r)
              ((->re `(:) c) z)
              ((->re `(& (& #\111 (^ (- #\000 #\110) (- #\112 #\377)))
                         (& (* #\2))) c)
               (build-and (list (build-char-set (make-range 73 73) c)
                                (build-repeat (build-char-set (make-range 50 50) c) c))
                          c))
              ((->re `(& (& #\000 (^ (- #\000 #\110) (- #\112 #\377)))
                         (& (* #\2))) c)
               z)
              ((->re `(& ,rr ,rrr) c) (build-and (list rr rrr) c))
              ((->re `(& ,r) c) r)
              ((->re `(&) c) (build-neg z c))
              ((->re `(~ ,r) c) (build-neg r c))
              ((->re `(@) c) e)
              ((->re `(@ ,rrr*) c) rrr*)
              (rr (build-concat r r c))
              ((->re `(@ ,r ,rr ,rrr) c)
               (build-concat r (build-concat rr rrr c) c))
              ((char-setR-chars (->re `(- #\1 #\1) c)) (make-range 49 49))
              ((char-setR-chars (->re `(- #\1 #\9) c)) (make-range 49 57))
              ((char-setR-chars (->re `(- "1" "1") c)) (make-range 49 49))
              ((char-setR-chars (->re `(- "1" "9") c)) (make-range 49 57))
              ((->re `(- "9" "1") c) z)
              ((char-setR-chars (->re `(^) c))
               (char-setR-chars (->re `(- #\000 #\377) c)))
              ((char-setR-chars (->re `(^ #\001 (- #\002 #\377)) c)) (make-range 0 0))
              )
  
  )