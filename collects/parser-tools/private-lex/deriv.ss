(module deriv mzscheme
  
  (require (lib "list.ss")
           "re.ss"
           "util.ss")

  (provide build-dfa print-dfa (struct dfa (num-states start-state final-states/actions transitions)))

  (define e (build-epsilon))
  (define z (build-zero))
  
  ;; get-char-groups : re -> (list-of char-setR?)
  ;; Collects the char-setRs in r that could be used in
  ;; taking the derivative of r.
  (define (get-char-groups r)
    (cond
      ((or (eq? r e) (eq? r z)) null)
      ((char-setR? r) (list r))
      ((concatR? r)
       (if (re-nullable? (concatR-re1 r))
           (append (get-char-groups (concatR-re1 r))
                   (get-char-groups (concatR-re2 r)))
           (get-char-groups (concatR-re1 r))))
      ((repeatR? r)
       (get-char-groups (repeatR-re r)))
      ((orR? r)
       (apply append (map get-char-groups (orR-res r))))))

  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c)))
              ((get-char-groups e) null)
              ((get-char-groups z) null)
              ((get-char-groups r1) (list r1))
              ((get-char-groups (->re `(@ ,r1 ,r2) c))
               (list r1))
              ((get-char-groups (->re `(@ ,e ,r2) c)) 
               (list r2))
              ((get-char-groups (->re `(@ (* ,r1) ,r2) c)) 
               (list r1 r2))
              ((get-char-groups (->re `(* ,r1) c)) 
               (list r1))
              ((get-char-groups (->re `(: (* ,r1) (@ (* ,r2) "3") "4") c))
               (list r1 r2 (->re "3" c) (->re "4" c)))
              )
               
  
  ;; A char-set is a (list-of char) that is sorted and duplicate-free
  
  ;; partition : (list-of char-set) -> (list-of char-set)
  ;; The coarsest refinment r of sets such that the char-sets in r
  ;; are pairwise disjoint.
  (define (partition sets)
    (cond
      ((null? sets) null)
      (else
       (partition1 (car sets) (partition (cdr sets))))))
    
  ;; partition1 : char-set (list-of char-set) -> (list-of char-set)
  ;; All the char-sets in sets must be pairwise disjoint.  Splits set
  ;; against each element in sets.
  (define (partition1 set sets)
    (cond
      ((null? set) sets)
      ((null? sets) (list set))
      (else
       (let ((set2 (car sets)))
         (let-values (((i s1 s2) (split set set2)))
           (let ((rest (partition1 s1 (cdr sets))))
             (cond
               ((null? i)
                (cons s2 rest))
               ((null? s2)
                (cons i rest))
               (else
                (cons i (cons s2 rest))))))))))
              
  (test-block ((sl string->list))
              ((partition null) null)
              ((partition (list (sl "1234"))) (list (sl "1234")))
              ((partition (list (sl "1234") (sl "0235")))
               (list (sl "23") (sl "05") (sl "14")))
              ((partition (list (sl "12349") (sl "02359") (sl "67") (sl "29")))
               (list (sl "29") (sl "67") (sl "3") (sl "05") (sl "14")))
              )
  
  (test-block ((sl string->list))
              ((partition1 (sl "bcdjw") null) (list (sl "bcdjw")))
              ((partition1 null null) null)
              ((partition1 null (list (sl "a") (sl "b") (sl "1")))
               (list (sl "a") (sl "b") (sl "1")))
              ((partition1 (sl "bcdjw")
                           (list (sl "z")
                                 (sl "ab")
                                 (sl "dj")))
               (list (sl "z") (sl "b") (sl "a") (sl "dj") (sl "cw"))))
              
                                 
  
  ;; deriveR : re * char cache -> re
  (define (deriveR r c cache)
    (cond
      ((or (eq? r e) (eq? r z)) z)
      ((char-setR? r)
       (if (memq c (char-setR-chars r)) e z))
      ((concatR? r)
       (let* ((r1 (concatR-re1 r))
              (r2 (concatR-re2 r))
              (d (build-concat (deriveR r1 c cache) r2 cache)))
         (if (re-nullable? r1)
             (build-or (list d (deriveR r2 c cache)) cache)
             d)))
      ((repeatR? r)
       (build-concat (deriveR (repeatR-re r) c cache) r cache))
      ((orR? r)
       (build-or (map (lambda (x) (deriveR x c cache))
                      (orR-res r))
                 cache))
      ((andR? r)
       (build-and (map (lambda (x) (deriveR x c cache))
                       (andR-res r))
                  cache))
      ((negR? r)
       (build-neg (deriveR (negR-re r) c cache) cache))))

  (test-block ((c (make-cache))
               (r1 (->re #\a c))
               (r2 (->re `(* #\a) c))
               (r3 (->re `(* ,r2) c))
               (r4 (->re `(@ #\a ,r2) c))
               (r5 (->re `(* ,r4) c))
               (r6 (->re `(: ,r5 #\a) c))
               (r7 (->re `(@ ,r2 ,r2) c)))
              ((deriveR e #\a c) z)
              ((deriveR z #\a c) z)
              ((deriveR r1 #\b c) z)
              ((deriveR r1 #\a c) e)
              ((deriveR r2 #\a c) r2)
              ((deriveR r2 #\b c) z)
              ((deriveR r3 #\a c) (->re `(@ ,r2 ,r3) c))
              ((deriveR r3 #\b c) z)
              ((deriveR r4 #\a c) r2)
              ((deriveR r4 #\b c) z)
              ((deriveR r5 #\a c) (->re `(@ ,r2 ,r5) c))
              ((deriveR r5 #\b c) z)
              ((deriveR r6 #\a c) (->re `(: (@ ,r2 ,r5) (epsilon)) c))
              ((deriveR r6 #\b c) z)
              ((deriveR r7 #\a c) (->re `(: (@ ,r2 ,r2) ,r2) c))
              ((deriveR r7 #\b c) z))
  
  
  ;; An re-action is (cons re action)

  ;; derive : (list-of re-action) char cache -> (union (list-of re-action) #f)
  ;; applies deriveR to all the re-actions's re parts.
  ;; Returns #f if the derived state is equivalent to z.
  (define (derive r c cache)
    (let ((new-r (map (lambda (ra)
                        (cons (deriveR (car ra) c cache) (cdr ra)))
                      r)))
      (if (andmap (lambda (x) (eq? z (car x)))
                  new-r)
          #f
          new-r)))

  (test-block ((c (make-cache))
               (r1 (->re #\1 c))
               (r2 (->re #\2 c)))
              ((derive null #\1 c) #f)
              ((derive (list (cons r1 1) (cons r2 2)) #\1 c)
               (list (cons e 1) (cons z 2)))
              ((derive (list (cons r1 1) (cons r2 2)) #\3 c) #f))
  

  ;; get-final : (list-of re-action) -> (union #f syntax-object)
  ;; An re that accepts e represents a final state.  Return the
  ;; action from the first final state or #f if there is none.
  (define (get-final res)
    (cond
      ((null? res) #f)
      ((re-nullable? (caar res)) (cdar res))
      (else (get-final (cdr res)))))

  (test-block ((c (make-cache))
               (r1 (->re #\a c))
               (r2 (->re #\b c))
               (a (list (cons r1 1) (cons r2 2))))
              ((derive null #\a c) #f)
              ((derive a #\a c) (list (cons e 1) (cons z 2)))
              ((derive a #\b c) (list (cons z 1) (cons e 2)))
              ((derive a #\c c) #f)
              ((get-final a) #f)
              ((get-final (list (cons e 1) (cons e 2))) 1))
  
  
  ;; A state is (make-state (list-of re-action) nat)
  (define-struct state (spec index))
        
  ;; get->key : re-action -> (list-of nat)
  ;; states are indexed by the list of indexes of their res
  (define (get-key s)
    (map (lambda (x) (re-index (car x))) s))  
          
  ;; compute-chars : (list-of state) -> (list-of char-set)
  ;; Computed the sets of equivalent characters for taking the
  ;; derivative of the car of st.  Only one derivative per set need to be taken.
  (define (compute-chars st)
    (cond
      ((null? st) null)
      (else
       (partition (map char-setR-chars
                       (apply append (map (lambda (x) (get-char-groups (car x)))
                                          (state-spec (car st)))))))))
  
  (test-block ((c (make-cache))
               (r1 (->re `(- #\1 #\4) c))
               (r2 (->re `(- #\2 #\3) c)))
              ((compute-chars null) null)
              ((compute-chars (list (make-state null 1))) null)
              ((compute-chars (list (make-state (list (cons r1 1) (cons r2 2)) 2)))
               (list (list #\2 #\3) (list #\1 #\4))))
  
  
  ;; A dfa is (make-dfa int int
  ;;                    (list-of (cons int syntax-object))
  ;;                    (list-of (cons int (list-of (cons (list-of char) int)))))
  ;; Each transitions is a state and a list of chars with the state to transition to.
  ;; The finals and transitions are sorted by state number, and duplicate free.
  (define-struct dfa (num-states start-state final-states/actions transitions))
  
  ;; build-dfa : (list-of re-action) cache -> dfa
  (define (build-dfa rs cache)
    (let* ((transitions (make-hash-table))
           (get-state-number (make-counter))
           (start (make-state rs (get-state-number))))
      (cache (cons 'state (get-key rs)) (lambda () start))
      (let loop ((old-states (list start))
                 (new-states null)
                 (cs (compute-chars (list start))))
        (cond
          ((and (null? old-states) (null? new-states))
           (make-dfa (get-state-number) (state-index start)
                     (mergesort (filter (lambda (x) (cdr x))
                                        (hash-table-map transitions
                                                        (lambda (state _)
                                                          (cons (state-index state) (get-final (state-spec state))))))
                                (lambda (a b) (< (car a) (car b))))
                     (mergesort (hash-table-map transitions
                                                (lambda (state trans)
                                                  (cons (state-index state)
                                                        (map (lambda (t)
                                                               (cons (car t)
                                                                     (state-index (cdr t))))
                                                             trans))))
                                (lambda (a b) (< (car a) (car b))))))
          ((null? old-states)
           (loop new-states null (compute-chars new-states)))
          ((null? cs)
           (loop (cdr old-states) new-states (compute-chars (cdr old-states))))
          (else
           (let* ((state (car old-states))
                  (c (car cs))
                  (new-re (derive (state-spec state) (car c) cache)))
             (cond
               (new-re
                (let* ((new-state? #f)
                       (new-state (cache (cons 'state (get-key new-re))
                                         (lambda ()
                                           (set! new-state? #t)
                                           (make-state new-re (get-state-number))))))
                  (hash-table-put! transitions 
                                   state
                                   (cons (cons c new-state)
                                         (hash-table-get transitions state
                                                         (lambda () null))))
                  (cond
                    (new-state?
                     (loop old-states (cons new-state new-states) (cdr cs)))
                    (else
                     (loop old-states new-states (cdr cs))))))
               (else (loop old-states new-states (cdr cs))))))))))
  
  (define (print-dfa x)
    (printf "number of states: ~a~n" (dfa-num-states x))
    (printf "start state: ~a~n" (dfa-start-state x))
    (printf "final states: ~a~n" (map car (dfa-final-states/actions x)))
    (for-each (lambda (trans)
                (printf "state: ~a~n" (car trans))
                (for-each (lambda (rule)
                            (printf "  -~a-> ~a~n"
                                    (car rule)
                                    (cdr rule)))
                          (cdr trans)))
              (dfa-transitions x)))
 
  (define (build-test-dfa rs)
    (let ((c (make-cache)))
      (build-dfa (map (lambda (x) (cons (->re x c) 'action))
                      rs)
                 c)))
  #|
  (define t1 (build-test-dfa null))
  (define t2 (build-test-dfa `(#\a)))
  (define t3 (build-test-dfa `(#\a #\b)))
  (define t4 (build-test-dfa `((* #\a)
                               (* (@ #\a #\b)))))
  (define t5 (build-test-dfa `((@ (* (: #\0 #\1)) #\1))))
  (define t6 (build-test-dfa `((* (* #\a))
                               (* (@ #\b (* #\b))))))
  (define t7 (build-test-dfa `((@ (* #\a) (* #\b) (* #\c) (* #\d) (* #\e)))))
  (define t8
    (build-test-dfa `((@ (* (: #\a #\b)) #\a (: #\a #\b) (: #\a #\b) (: #\a #\b) (: #\a #\b)))))
|#
  )