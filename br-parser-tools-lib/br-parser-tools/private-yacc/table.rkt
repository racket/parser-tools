#lang racket/base
(require "grammar.rkt"
         "lr0.rkt"
         "lalr.rkt"
         "parser-actions.rkt"
         racket/contract
         racket/list
         racket/class)

;; Routine to build the LALR table


(define (is-a-grammar%? x) (is-a? x grammar%))
(provide/contract 
 (build-table (-> is-a-grammar%? string? any/c
                  (vectorof (listof (cons/c (or/c term? non-term?) action?))))))

;; A parse-table is (vectorof (listof (cons/c gram-sym? action)))
;; A grouped-parse-table is (vectorof (listof (cons/c gram-sym? (listof action))))
  
;; make-parse-table : int -> parse-table
(define (make-parse-table num-states)
  (make-vector num-states null))
  
;; table-add!: parse-table nat symbol action ->
(define (table-add! table state-index symbol val)
  (vector-set! table state-index (cons (cons symbol val)
                                       (vector-ref table state-index))))
  
;; group-table : parse-table -> grouped-parse-table
(define (group-table table)
  (list->vector
   (for/list ([state-entry (in-list (vector->list table))])
             (define ht (make-hasheq))
             (for* ([gs/actions (in-list state-entry)]
                    [group (in-value (hash-ref ht (car gs/actions) (λ () null)))]
                    #:unless (member (cdr gs/actions) group))
                   (hash-set! ht (car gs/actions) (cons (cdr gs/actions) group)))
             (hash-map ht cons))))
  
;; table-map : (vectorof (listof (cons/c gram-sym? X))) (gram-sym? X -> Y) ->
;;             (vectorof (listof (cons/c gram-sym? Y)))
(define (table-map f table)
  (list->vector
   (for/list ([state-entry (in-list (vector->list table))])
             (for/list ([gs/X (in-list state-entry)])
                       (cons (car gs/X) (f (car gs/X) (cdr gs/X)))))))
  
(define (bit-vector-for-each f bv)
  (let loop ([bv bv] [number 0])
    (cond
      [(zero? bv) (void)]
      [(= 1 (bitwise-and 1 bv))
       (f number)
       (loop (arithmetic-shift bv -1) (add1 number))]
      [else (loop (arithmetic-shift bv -1) (add1 number))])))
                   
  
;; print-entry: symbol action output-port ->
;; prints the action a for lookahead sym to the given port
(define (print-entry sym a port)
  (define s "\t~a\t\t\t\t\t~a\t~a\n")
  (cond
    [(shift? a) (fprintf port s sym "shift" (shift-state a))]
    [(reduce? a) (fprintf port s sym "reduce" (prod-index (reduce-prod a)))]
    [(accept? a) (fprintf port s sym "accept" "")]
    [(goto? a) (fprintf port s sym "goto" (goto-state a))]))


;; count: ('a -> bool) * 'a list -> num
;; counts the number of elements in list that satisfy pred
(define (count pred list)
  (cond
    [(null? list) 0]
    [(pred (car list)) (+ 1 (count pred (cdr list)))]
    [else (count pred (cdr list))]))

;; display-parser: LR0-automaton grouped-parse-table (listof prod?) output-port ->
;; Prints out the parser given by table.
(define (display-parser a grouped-table prods port)
  (define SR-conflicts 0)
  (define RR-conflicts 0)
  (for ([prod (in-list prods)])
       (fprintf port 
                "~a\t~a\t=\t~a\n" 
                (prod-index prod)
                (gram-sym-symbol (prod-lhs prod))
                (map gram-sym-symbol (vector->list (prod-rhs prod)))))
  
  (send a for-each-state
        (λ (state)
          (fprintf port "State ~a\n" (kernel-index state))
          (for ([item (in-list (kernel-items state))])
               (fprintf port "\t~a\n" (item->string item)))
          (newline port)
          (for ([gs/action (in-list (vector-ref grouped-table (kernel-index state)))])
               (define sym (gram-sym-symbol (car gs/action)))
               (define act (cdr gs/action))
               (cond
                 [(null? act) (void)]
                 [(null? (cdr act))
                  (print-entry sym (car act) port)]
                 [else
                  (fprintf port "begin conflict:\n")
                  (when (> (count reduce? act) 1)
                    (set! RR-conflicts (add1 RR-conflicts)))
                  (when (> (count shift? act) 0)
                    (set! SR-conflicts (add1 SR-conflicts)))
                  (map (λ (x) (print-entry sym x port)) act)
                  (fprintf port "end conflict\n")]))
          (newline port)))
      
  (when (> SR-conflicts 0)
    (fprintf port "~a shift/reduce conflict~a\n" 
             SR-conflicts
             (if (= SR-conflicts 1) "" "s")))
  (when (> RR-conflicts 0)
    (fprintf port "~a reduce/reduce conflict~a\n"
             RR-conflicts
             (if (= RR-conflicts 1) "" "s"))))
  
;; resolve-conflict : (listof action?) -> action? bool bool
(define (resolve-conflict actions)
  (cond
    [(null? actions) (values (make-no-action) #f #f)]
    [(null? (cdr actions)) (values (car actions) #f #f)]
    [else
     (define SR-conflict? (> (count shift? actions) 0))
     (define RR-conflict? (> (count reduce? actions) 1))
     (let loop ((current-guess #f)
                (rest actions))
       (cond
         [(null? rest) (values current-guess SR-conflict? RR-conflict?)]
         [(shift? (car rest)) (values (car rest) SR-conflict? RR-conflict?)]
         [(not current-guess) (loop (car rest) (cdr rest))]
         [(and (reduce? (car rest))
               (< (prod-index (reduce-prod (car rest)))
                  (prod-index (reduce-prod current-guess))))
          (loop (car rest) (cdr rest))]
         [(accept? (car rest))
          (eprintf "accept/reduce or accept/shift conflicts.  Check the grammar for useless cycles of productions\n")
          (loop current-guess (cdr rest))]
         [else (loop current-guess (cdr rest))]))]))
  
;; resolve-conflicts : grouped-parse-table bool -> parse-table
(define (resolve-conflicts grouped-table suppress)
  (define SR-conflicts 0)
  (define RR-conflicts 0)
  (define table (table-map
                 (λ (gs actions)
                   (let-values ([(action SR? RR?)
                                 (resolve-conflict actions)])
                     (when SR?
                       (set! SR-conflicts (add1 SR-conflicts)))
                     (when RR?
                       (set! RR-conflicts (add1 RR-conflicts)))
                     action))
                 grouped-table))
  (unless suppress
    (when (> SR-conflicts 0)
      (eprintf "~a shift/reduce conflict~a\n"
               SR-conflicts
               (if (= SR-conflicts 1) "" "s")))
    (when (> RR-conflicts 0)
      (eprintf "~a reduce/reduce conflict~a\n"
               RR-conflicts
               (if (= RR-conflicts 1) "" "s"))))
  table)
  

;; resolve-sr-conflict : (listof action) (union int #f) -> (listof action)
;; Resolves a single shift-reduce conflict, if precedences are in place.
(define (resolve-sr-conflict/prec actions shift-prec)
  (define shift (if (shift? (car actions))
                    (car actions)
                    (cadr actions)))
  (define reduce (if (shift? (car actions))
                     (cadr actions)
                     (car actions)))
  (define reduce-prec (prod-prec (reduce-prod reduce)))
  (cond
    [(and shift-prec reduce-prec)
     (cond
       [(< (prec-num shift-prec) (prec-num reduce-prec))
        (list reduce)]
       [(> (prec-num shift-prec) (prec-num reduce-prec))
        (list shift)]
       [(eq? 'left (prec-assoc shift-prec))
        (list reduce)]
       [(eq? 'right (prec-assoc shift-prec))
        (list shift)]
       [else null])]
    [else actions]))
    
  
;; resolve-prec-conflicts : parse-table -> grouped-parse-table
(define (resolve-prec-conflicts table)
  (table-map
   (λ (gs actions)
     (cond
       [(and (term? gs)
             (= 2 (length actions))
             (or (shift? (car actions))
                 (shift? (cadr actions))))
        (resolve-sr-conflict/prec actions (term-prec gs))]
       [else actions]))
   (group-table table)))
                        
;; build-table: grammar string bool -> parse-table
(define (build-table g file suppress)
  (define a (build-lr0-automaton g))
  (define term-vector (list->vector (send g get-terms)))
  (define end-terms (send g get-end-terms))
  (define table (make-parse-table (send a get-num-states)))
  (define get-lookahead (compute-LA a g))
  (define reduce-cache (make-hash))
  (for ([trans-key/state (in-list (send a get-transitions))])
       (define from-state-index (kernel-index (trans-key-st (car trans-key/state))))
       (define gs (trans-key-gs (car trans-key/state)))
       (define to-state (cdr trans-key/state))
       
       (table-add! table from-state-index gs
                   (cond
                     ((non-term? gs)
                      (make-goto (kernel-index to-state)))
                     ((member gs end-terms)
                      (make-accept))
                     (else
                      (make-shift 
                       (kernel-index to-state))))))
  (send a for-each-state
        (λ (state)
          (for ([item (in-list (append (hash-ref (send a get-epsilon-trans) state (λ () null))
                                       (filter (λ (item)
                                                 (not (move-dot-right item)))
                                               (kernel-items state))))])
               (let ([item-prod (item-prod item)])
                 (bit-vector-for-each 
                  (λ (term-index)
                    (unless (start-item? item)
                      (let ((r (hash-ref reduce-cache item-prod
                                         (λ ()
                                           (let ((r (make-reduce item-prod)))
                                             (hash-set! reduce-cache item-prod r)
                                             r)))))
                        (table-add! table
                                    (kernel-index state)
                                    (vector-ref term-vector term-index)
                                    r))))
                  (get-lookahead state item-prod))))))

  (define grouped-table (resolve-prec-conflicts table))
  (unless (string=? file "")
    (with-handlers [(exn:fail:filesystem?
                     (λ (e)
                       (eprintf
                        "Cannot write debug output to file \"~a\": ~a\n"
                        file
                        (exn-message e))))]
      (call-with-output-file file
        (λ (port)
          (display-parser a grouped-table (send g get-prods) port))
        #:exists 'truncate)))
  (resolve-conflicts grouped-table suppress))
