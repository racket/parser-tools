#lang racket/base  
(require "grammar.rkt"
         "graph.rkt"
         racket/list
         racket/class)

;; Handle the LR0 automaton
  
(provide build-lr0-automaton lr0%
         (struct-out trans-key) trans-key-list-remove-dups
         kernel-items kernel-index)

;; kernel = (make-kernel (LR1-item list) index)
;;   the list must be kept sorted according to item<? so that equal? can
;;   be used to compare kernels
;;   Each kernel is assigned a unique index, 0 <= index < number of states
;; trans-key = (make-trans-key kernel gram-sym)
(define-struct kernel (items index) #:inspector (make-inspector))
(define-struct trans-key (st gs) #:inspector (make-inspector))

(define (trans-key<? a b)
  (define kia (kernel-index (trans-key-st a)))
  (define kib (kernel-index (trans-key-st b)))
  (or (< kia kib)
      (and (= kia kib)
           (< (non-term-index (trans-key-gs a))
              (non-term-index (trans-key-gs b))))))
  
(define (trans-key-list-remove-dups tkl)
  (let loop ([sorted (sort tkl trans-key<?)])
    (cond
      [(null? sorted) null]
      [(null? (cdr sorted)) sorted]
      [else
       (if (and (= (non-term-index (trans-key-gs (car sorted)))
                   (non-term-index (trans-key-gs (cadr sorted))))
                (= (kernel-index (trans-key-st (car sorted)))
                   (kernel-index (trans-key-st (cadr sorted)))))
           (loop (cdr sorted))
           (cons (car sorted) (loop (cdr sorted))))])))


;; build-transition-table : int (listof (cons/c trans-key X) ->
;;                          (vectorof (symbol X hashtable))
(define (build-transition-table num-states assoc)
  (define transitions (make-vector num-states #f))
  (let loop ([i (sub1 (vector-length transitions))])
    (when (>= i 0)
      (vector-set! transitions i (make-hasheq))
      (loop (sub1 i))))
  (for ([trans-key/kernel (in-list assoc)])
       (define tk (car trans-key/kernel))
       (hash-set! (vector-ref transitions (kernel-index (trans-key-st tk)))
                  (gram-sym-symbol (trans-key-gs tk))
                  (cdr trans-key/kernel)))
  transitions)
  
;; reverse-assoc : (listof (cons/c trans-key? kernel?)) ->
;;                 (listof (cons/c trans-key? (listof kernel?)))
(define (reverse-assoc assoc)
  (define reverse-hash (make-hash))
  (define (hash-table-add! ht k v)
    (hash-set! ht k (cons v (hash-ref ht k (λ () null)))))
  (for ([trans-key/kernel (in-list assoc)])
       (define tk (car trans-key/kernel))
       (hash-table-add! reverse-hash 
                        (make-trans-key (cdr trans-key/kernel)
                                        (trans-key-gs tk))
                        (trans-key-st tk)))
  (hash-map reverse-hash cons))


;; kernel-list-remove-duplicates
;; LR0-automaton = object of class lr0%
(define lr0%
  (class object%
    (super-instantiate ())
    ;; term-assoc : (listof (cons/c trans-key? kernel?))
    ;; non-term-assoc : (listof (cons/c trans-key? kernel?))
    ;; states : (vectorof kernel?)
    ;; epsilons : ???
    (init-field term-assoc non-term-assoc states epsilons)
            
    (define transitions (build-transition-table (vector-length states)
                                                (append term-assoc non-term-assoc)))
      
    (define reverse-term-assoc (reverse-assoc term-assoc))
    (define reverse-non-term-assoc (reverse-assoc non-term-assoc))
    (define reverse-transitions
      (build-transition-table (vector-length states)
                              (append reverse-term-assoc reverse-non-term-assoc)))
      
    (define mapped-non-terms (map car non-term-assoc))
      
    (define/public (get-mapped-non-term-keys)
      mapped-non-terms)

    (define/public (get-num-states)
      (vector-length states))
      
    (define/public (get-epsilon-trans)
      epsilons)

    (define/public (get-transitions)
      (append term-assoc non-term-assoc))
      
    ;; for-each-state : (state ->) ->
    ;; Iteration over the states in an automaton
    (define/public (for-each-state f)
      (define num-states (vector-length states))
      (let loop ([i 0])
        (when (< i num-states)
          (f (vector-ref states i))
          (loop (add1 i)))))
      
    ;; run-automaton: kernel? gram-sym? -> (union kernel #f)
    ;; returns the state reached from state k on input s, or #f when k
    ;; has no transition on s
    (define/public (run-automaton k s)
      (hash-ref (vector-ref transitions (kernel-index k))
                (gram-sym-symbol s)
                (λ () #f)))

    ;; run-automaton-back : (listof kernel?) gram-sym? -> (listof kernel)
    ;; returns the list of states that can reach k by transitioning on s.
    (define/public (run-automaton-back k s)
      (for*/list ([k (in-list k)]
                  [val (in-list (hash-ref (vector-ref reverse-transitions (kernel-index k))
                                          (gram-sym-symbol s)
                                          (λ () null)))])
                 val))))

(define ((union comp<?) l1 l2)
  (let loop ([l1 l1] [l2 l2])
    (cond
      [(null? l1) l2]
      [(null? l2) l1]
      [else (define c1 (car l1))
            (define c2 (car l2))
            (cond
              [(comp<? c1 c2) (cons c1 (loop (cdr l1) l2))]
              [(comp<? c2 c1) (cons c2 (loop l1 (cdr l2)))]
              [else (loop (cdr l1) l2)])])))
  

;; The kernels in the automaton are represented cannonically.
;; That is (equal? a b) <=> (eq? a b)
(define (kernel->string k)
  (apply string-append 
         `("{" ,@(map (λ (i) (string-append (item->string i) ", ")) 
                      (kernel-items k)) 
               "}")))

;; build-LR0-automaton: grammar -> LR0-automaton
;; Constructs the kernels of the sets of LR(0) items of g
(define (build-lr0-automaton grammar)
  ;    (printf "LR(0) automaton:\n")
  (define epsilons (make-hash))
  (define grammar-symbols (append (send grammar get-non-terms)
                                  (send grammar get-terms)))
  ;; first-non-term: non-term -> non-term list
  ;; given a non-terminal symbol C, return those non-terminal 
  ;; symbols A s.t. C -> An for some string of terminals and
  ;; non-terminals n where -> means a rightmost derivation in many 
  ;; steps.  Assumes that each non-term can be reduced to a string 
  ;; of terms.
  (define first-non-term 
    (digraph (send grammar get-non-terms)
             (λ (nt)
               (filter non-term?
                       (map (λ (prod) (sym-at-dot (make-item prod 0)))
                            (send grammar get-prods-for-non-term nt))))
             (λ (nt) (list nt))
             (union non-term<?)
             (λ () null)))
             
  ;; closure: LR1-item list -> LR1-item list
  ;; Creates a set of items containing i s.t. if A -> n.Xm is in it,
  ;; X -> .o is in it too.
  (define (LR0-closure i)
    (cond
      [(null? i) null]
      [else
       (define next-gsym (sym-at-dot (car i)))
       (cond
         [(non-term? next-gsym)
          (cons (car i)
                (append
                 (for*/list ([non-term (in-list (first-non-term next-gsym))]
                             [x (in-list (send grammar 
                                               get-prods-for-non-term
                                               non-term))])
                            (make-item x 0))
                 (LR0-closure (cdr i))))]
         [else (cons (car i) (LR0-closure (cdr i)))])]))
  
  ;; maps trans-keys to kernels
  (define automaton-term null)
  (define automaton-non-term null)
             
  ;; keeps the kernels we have seen, so we can have a unique
  ;; list for each kernel
  (define kernels (make-hash))
  (define counter 0)
             
  ;; goto: LR1-item list -> LR1-item list list
  ;; creates new kernels by moving the dot in each item in the
  ;; LR0-closure of kernel to the right, and grouping them by 
  ;; the term/non-term moved over.  Returns the kernels not
  ;; yet seen, and places the trans-keys into automaton
  (define (goto kernel)
    ;; maps a gram-syms to a list of items
    (define table (make-hasheq))

    ;; add-item!: 
    ;;   (symbol (listof item) hashtable) item? ->
    ;; adds i into the table grouped with the grammar
    ;; symbol following its dot
    (define (add-item! table i)
      (define gs (sym-at-dot i))
      (cond
        [gs (define already (hash-ref table (gram-sym-symbol gs) (λ () null)))
            (unless (member i already)
              (hash-set! table (gram-sym-symbol gs) (cons i already)))]
        ((zero? (vector-length (prod-rhs (item-prod i))))
         (define current (hash-ref epsilons kernel (λ () null)))
         (hash-set! epsilons kernel (cons i current)))))
    
    ;; Group the items of the LR0 closure of the kernel
    ;; by the character after the dot
    (for ([item (in-list (LR0-closure (kernel-items kernel)))])
         (add-item! table item))
                  
    ;; each group is a new kernel, with the dot advanced.
    ;; sorts the items in a kernel so kernels can be compared
    ;; with equal? for using the table kernels to make sure
    ;; only one representitive of each kernel is created
    (define is
      (let loop ([gsyms grammar-symbols])
        (cond
          [(null? gsyms) null]
          [else
           (define items (hash-ref table (gram-sym-symbol (car gsyms)) (λ () null)))
           (cond
             [(null? items) (loop (cdr gsyms))]
             [else (cons (list (car gsyms) items)
                         (loop (cdr gsyms)))])])))
    (filter
     values
     (for/list ([i (in-list is)])
               (define gs (car i))
               (define items (cadr i))
               (define new #f)
               (define new-kernel (sort (filter values (map move-dot-right items)) item<?))
               (define unique-kernel (hash-ref kernels new-kernel
                                               (λ ()
                                                 (define k (make-kernel new-kernel counter))
                                                 (set! new #t)
                                                 (set! counter (add1 counter))
                                                 (hash-set! kernels new-kernel k)
                                                 k)))
               (if (term? gs)
                   (set! automaton-term (cons (cons (make-trans-key kernel gs)
                                                    unique-kernel)
                                              automaton-term))
                   (set! automaton-non-term (cons (cons (make-trans-key kernel gs)
                                                        unique-kernel)
                                                  automaton-non-term)))
               #;(printf "~a -> ~a on ~a\n" 
                         (kernel->string kernel)
                         (kernel->string unique-kernel)
                         (gram-sym-symbol gs))
               (and new unique-kernel))))

  (define starts  (map (λ (init-prod) (list (make-item init-prod 0)))
                       (send grammar get-init-prods)))
  (define startk (for/list ([start (in-list starts)])
                           (define k (make-kernel start counter))
                           (hash-set! kernels start k)
                           (set! counter (add1 counter))
                           k))
  (define new-kernels (make-queue))
  (let loop ([old-kernels startk]
             [seen-kernels null])
    (cond
      [(and (empty-queue? new-kernels) (null? old-kernels))
       (make-object lr0%  automaton-term automaton-non-term
         (list->vector (reverse seen-kernels)) epsilons)]
      [(null? old-kernels) (loop (deq! new-kernels) seen-kernels)]
      [else 
       (enq! new-kernels (goto (car old-kernels)))
       (loop (cdr old-kernels) (cons (car old-kernels) seen-kernels))])))

(define-struct q (f l) #:inspector (make-inspector) #:mutable)
(define (empty-queue? q) (null? (q-f q)))
(define (make-queue) (make-q null null))

(define (enq! q i)
  (cond
    [(empty-queue? q)
     (let ([i (mcons i null)])
       (set-q-l! q i)
       (set-q-f! q i))]
    [else
     (set-mcdr! (q-l q) (mcons i null))
     (set-q-l! q (mcdr (q-l q)))]))


(define (deq! q)
  (begin0
    (mcar (q-f q))
    (set-q-f! q (mcdr (q-f q)))))


