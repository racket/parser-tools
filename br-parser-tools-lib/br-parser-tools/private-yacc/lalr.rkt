#lang racket/base
(require "lr0.rkt"
         "grammar.rkt"
         racket/list
         racket/class)

;; Compute LALR lookaheads from DeRemer and Pennello 1982

(provide compute-LA)
  
;; compute-DR: LR0-automaton * grammar -> (trans-key -> term set)
;; computes for each state, non-term transition pair, the terminals
;; which can transition out of the resulting state
;; output term set is represented in bit-vector form
(define ((compute-DR a g) tk)
  (define r (send a run-automaton (trans-key-st tk) (trans-key-gs tk)))
  (term-list->bit-vector
   (filter (λ (term) (send a run-automaton r term)) (send g get-terms))))
  
;; compute-reads: 
;;   LR0-automaton * grammar -> (trans-key -> trans-key list)
(define (compute-reads a g)
  (define nullable-non-terms  (filter (λ (nt) (send g nullable-non-term? nt)) (send g get-non-terms)))
  (λ (tk)
    (define r (send a run-automaton (trans-key-st tk) (trans-key-gs tk)))
    (for/list ([non-term (in-list nullable-non-terms)]
               #:when (send a run-automaton r non-term))
              (make-trans-key r non-term))))
  
;; compute-read: LR0-automaton * grammar -> (trans-key -> term set)
;; output term set is represented in bit-vector form
(define (compute-read a g)
  (define dr (compute-DR a g))
  (define reads (compute-reads a g))
  (digraph-tk->terml (send a get-mapped-non-term-keys)
                     reads
                     dr
                     (send a get-num-states)))
;; returns the list of all k such that state k transitions to state start on the
;; transitions in rhs (in order)
(define (run-lr0-backward a rhs dot-pos start num-states)
  (let loop ([states (list start)]
             [i (sub1 dot-pos)])
    (cond
      [(< i 0) states]
      [else (loop (send a run-automaton-back states (vector-ref rhs i))
                  (sub1 i))])))

;; prod->items-for-include: grammar * prod * non-term -> lr0-item list
;; returns the list of all (B -> beta . nt gamma) such that prod = (B -> beta nt gamma)
;; and gamma =>* epsilon
(define (prod->items-for-include g prod nt)
  (define rhs (prod-rhs prod))
  (define rhs-l (vector-length rhs))
  (append (if (and (> rhs-l 0) (eq? nt (vector-ref rhs (sub1 rhs-l))))
              (list (make-item prod (sub1 rhs-l)))
              null)
          (let loop ([i (sub1 rhs-l)])
            (cond
              [(and (> i 0) 
                    (non-term? (vector-ref rhs i)) 
                    (send g nullable-non-term? (vector-ref rhs i)))
               (if (eq? nt (vector-ref rhs (sub1 i)))
                   (cons (make-item prod (sub1 i))
                         (loop (sub1 i)))
                   (loop (sub1 i)))]
              [else null]))))

;; prod-list->items-for-include: grammar * prod list * non-term -> lr0-item list
;; return the list of all (B -> beta . nt gamma) such that  (B -> beta nt gamma) in prod-list
;; and gamma =>* epsilon
(define (prod-list->items-for-include g prod-list nt)
  (apply append (map (λ (prod) (prod->items-for-include g prod nt)) prod-list)))

;; comput-includes: lr0-automaton * grammar -> (trans-key -> trans-key list)
(define (compute-includes a g)
  (define num-states (send a get-num-states))
  (define items-for-input-nt (make-vector (send g get-num-non-terms) null))
  (for ([input-nt (in-list (send g get-non-terms))])
       (vector-set! items-for-input-nt (non-term-index input-nt)
                    (prod-list->items-for-include g (send g get-prods) input-nt)))
  (λ (tk)
    (define goal-state (trans-key-st tk))
    (define non-term (trans-key-gs tk))
    (define items (vector-ref items-for-input-nt (non-term-index non-term)))
    (trans-key-list-remove-dups
     (apply append
            (for/list ([item (in-list items)])
                      (define prod (item-prod item))
                      (define rhs (prod-rhs prod))
                      (define lhs (prod-lhs prod))
                      (map (λ (state) (make-trans-key state lhs))
                           (run-lr0-backward a 
                                             rhs
                                             (item-dot-pos item)
                                             goal-state 
                                             num-states)))))))
  
;; compute-lookback: lr0-automaton * grammar -> (kernel * proc -> trans-key list)
(define (compute-lookback a g)
  (define num-states (send a get-num-states))
  (λ (state prod)
    (map (λ (k) (make-trans-key k (prod-lhs prod)))
         (run-lr0-backward a (prod-rhs prod) (vector-length (prod-rhs prod)) state num-states))))
  
;; compute-follow:  LR0-automaton * grammar -> (trans-key -> term set)
;; output term set is represented in bit-vector form
(define (compute-follow a g includes)
  (define read (compute-read a g))
  (digraph-tk->terml (send a get-mapped-non-term-keys)
                     includes
                     read
                     (send a get-num-states)))
    
;; compute-LA: LR0-automaton * grammar -> kernel * prod -> term set
;; output term set is represented in bit-vector form
(define (compute-LA a g)
  (define includes (compute-includes a g))
  (define lookback (compute-lookback a g))
  (define follow  (compute-follow a g includes))
  (λ (k p)
    (define l (lookback k p))
    (define f (map follow l))
    (apply bitwise-ior (cons 0 f))))


(define (print-DR dr a g)
  (print-input-st-sym dr "DR" a g print-output-terms))
(define (print-Read Read a g)
  (print-input-st-sym Read "Read" a g print-output-terms))
(define (print-includes i a g)
  (print-input-st-sym i "includes" a g print-output-st-nt))
(define (print-lookback l a g)
  (print-input-st-prod l "lookback" a g print-output-st-nt))
(define (print-follow f a g)
  (print-input-st-sym f "follow" a g print-output-terms))
(define (print-LA l a g)
  (print-input-st-prod l "LA" a g print-output-terms))

(define (print-input-st-sym f name a g print-output)
  (printf "~a:\n" name)
  (send a for-each-state
        (λ (state)
          (for-each
           (λ (non-term)
             (let ([res (f (make-trans-key state non-term))])
               (when (not (null? res))
                 (printf "~a(~a, ~a) = ~a\n"
                         name
                         state
                         (gram-sym-symbol non-term)
                         (print-output res)))))
           (send g get-non-terms))))
  (newline))

(define (print-input-st-prod f name a g print-output)
  (printf "~a:\n" name)
  (send a for-each-state
        (λ (state)
          (for-each
           (λ (non-term)
             (for-each
              (λ (prod)
                (let ([res (f state prod)])
                  (when (not (null? res))
                    (printf "~a(~a, ~a) = ~a\n"
                            name
                            (kernel-index state)
                            (prod-index prod)
                            (print-output res)))))
              (send g get-prods-for-non-term non-term)))
           (send g get-non-terms)))))
  
(define (print-output-terms r)
  (map gram-sym-symbol r))
  
(define (print-output-st-nt r)
  (map (λ (p) (list (kernel-index (trans-key-st p)) (gram-sym-symbol (trans-key-gs p)))) r))

;; init-tk-map : int -> (vectorof hashtable?)
(define (init-tk-map n)
  (define v (make-vector n #f))
  (let loop ([i (sub1 (vector-length v))])
    (when (>= i 0)
      (vector-set! v i (make-hasheq))
      (loop (sub1 i))))
  v)
  
;; lookup-tk-map : (vectorof (symbol? int hashtable)) -> trans-key? -> int
(define ((lookup-tk-map map) tk)
  (define st (trans-key-st tk))
  (define gs (trans-key-gs tk))
  (hash-ref (vector-ref map (kernel-index st))
            (gram-sym-symbol gs)
            (λ () 0)))

;; add-tk-map : (vectorof (symbol? int hashtable)) -> trans-key int -> 
(define ((add-tk-map map) tk v)
  (define st (trans-key-st tk))
  (define gs (trans-key-gs tk))
  (hash-set! (vector-ref map (kernel-index st))
             (gram-sym-symbol gs)
             v))
             
;; digraph-tk->terml: 
;;   (trans-key list) * (trans-key -> trans-key list) * (trans-key -> term list) * int * int * int
;;     -> (trans-key -> term list)
;; DeRemer and Pennello 1982
;; Computes (f x) = (f- x) union Union{(f y) | y in (edges x)}
;; A specialization of digraph in the file graph.rkt
(define (digraph-tk->terml nodes edges f- num-states)
  ;; Will map elements of trans-key to term sets represented as bit vectors
  (define results (init-tk-map num-states))
             
  ;; Maps elements of trans-keys to integers.
  (define N (init-tk-map num-states))
             
  (define get-N (lookup-tk-map N))
  (define set-N (add-tk-map N))
  (define get-f (lookup-tk-map results))
  (define set-f (add-tk-map results))
             
  (define stack null)
  (define (push x) (set! stack (cons x stack)))
  (define (pop) (begin0 
                  (car stack)
                  (set! stack (cdr stack))))
  (define (depth) (length stack))

  ;; traverse: 'a -> 
  (define (traverse x)
    (push x)
    (let ([d (depth)])
      (set-N x d)
      (set-f x (f- x))
      (for-each (λ (y)
                  (when (= 0 (get-N y))
                    (traverse y))
                  (set-f x (bitwise-ior (get-f x) (get-f y)))
                  (set-N x (min (get-N x) (get-N y))))
                (edges x))
      (when (= d (get-N x))
        (let loop ([p (pop)])
          (set-N p +inf.0)
          (set-f p (get-f x))
          (unless (equal? x p)
            (loop (pop)))))))

  (for ([x (in-list nodes)]
        #:when (zero? (get-N x)))
       (traverse x))
  get-f)
