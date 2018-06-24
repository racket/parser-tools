#lang racket/base
(require racket/base
         racket/match
         (prefix-in is: data/integer-set)
         racket/list
         syntax/stx
         "util.rkt"
         "stx.rkt"
         "re.rkt"
         "deriv.rkt")
  
(provide build-lexer)
  
(define-syntax time-label
  (syntax-rules ()
    ((_ l e ...)
     (begin
       (printf "~a: " l)
       (time (begin e ...))))))
  
;; A table is either
;; - (vector-of (union #f nat))
;; - (vector-of (vector-of (vector nat nat nat)))
  
(define loc:integer-set-contents is:integer-set-contents)
  
;; dfa->1d-table : dfa -> (same as build-lexer)
(define (dfa->1d-table dfa)
  (define state-table (make-vector (dfa-num-states dfa) #f))
  (define transition-cache (make-hasheq))
  (for ([trans (in-list (dfa-transitions dfa))])
       (match-define (cons from-state all-chars/to) trans)
       (define flat-all-chars/to
         (sort
          (for*/list ([chars/to (in-list all-chars/to)]
                      [char-ranges (in-value (loc:integer-set-contents (car chars/to)))]
                      [to (in-value (cdr chars/to))]
                      [char-range (in-list char-ranges)])
                     (define entry (vector (car char-range) (cdr char-range) to))
                     (hash-ref transition-cache entry (λ ()
                                                        (hash-set! transition-cache
                                                                   entry
                                                                   entry)
                                                        entry)))
          < #:key (λ (v) (vector-ref v 0))))
       (vector-set! state-table from-state (list->vector flat-all-chars/to)))
  state-table)
  
         
(define loc:foldr is:foldr)
  
;; dfa->2d-table : dfa -> (same as build-lexer)
(define (dfa->2d-table dfa)
  ;; char-table : (vector-of (union #f nat))
  ;; The lexer table, one entry per state per char.
  ;; Each entry specifies a state to transition to.
  ;; #f indicates no transition
  (define char-table (make-vector (* 256 (dfa-num-states dfa)) #f))
  ;; Fill the char-table vector
  (for* ([trans (in-list (dfa-transitions dfa))]
         [chars/to (in-list (cdr trans))])
        (define from-state (car trans))
        (define to-state (cdr chars/to))
        (loc:foldr (λ (char _)
                     (vector-set! char-table
                                  (bitwise-ior 
                                   char
                                   (arithmetic-shift from-state 8))
                                  to-state))
                   (void)
                   (car chars/to)))
  char-table)
      

;; dfa->actions : dfa -> (vector-of (union #f syntax-object))
;; The action for each final state, #f if the state isn't final
(define (dfa->actions dfa)
  (define actions (make-vector (dfa-num-states dfa) #f))
  (for ([state/action (in-list (dfa-final-states/actions dfa))])
       (vector-set! actions (car state/action) (cdr state/action)))
  actions)
      
;; dfa->no-look : dfa -> (vector-of bool)
;; For each state whether the lexer can ignore the next input.
;; It can do this only if there are no transitions out of the
;; current state.
(define (dfa->no-look dfa)
  (define no-look (make-vector (dfa-num-states dfa) #t))
  (for ([trans (in-list (dfa-transitions dfa))])
       (vector-set! no-look (car trans) #f))
  no-look)
      
(test-block ((d1 (make-dfa 1 1 (list) (list)))
             (d2 (make-dfa 4 1 (list (cons 2 2) (cons 3 3))
                           (list (cons 1 (list (cons (is:make-range 49 50) 1)
                                               (cons (is:make-range 51) 2)))
                                 (cons 2 (list (cons (is:make-range 49) 3))))))
             (d3 (make-dfa 4 1 (list (cons 2 2) (cons 3 3))
                           (list (cons 1 (list (cons (is:make-range 100 200) 0)
                                               (cons (is:make-range 49 50) 1)
                                               (cons (is:make-range 51) 2)))
                                 (cons 2 (list (cons (is:make-range 49) 3)))))))
            ((dfa->2d-table d1) (make-vector 256 #f))
            ((dfa->2d-table d2) (let ((v (make-vector 1024 #f)))
                                  (vector-set! v 305 1)
                                  (vector-set! v 306 1)
                                  (vector-set! v 307 2)
                                  (vector-set! v 561 3)
                                  v))
            ((dfa->1d-table d1) (make-vector 1 #f))
            ((dfa->1d-table d2) #(#f
                                  #(#(49 50 1) #(51 51 2))
                                  #(#(49 49 3))
                                  #f))
            ((dfa->1d-table d3) #(#f
                                  #(#(49 50 1) #(51 51 2) #(100 200 0))
                                  #(#(49 49 3))
                                  #f))
            ((dfa->actions d1) (vector #f))
            ((dfa->actions d2) (vector #f #f 2 3))
            ((dfa->no-look d1) (vector #t))
            ((dfa->no-look d2) (vector #t #f #f #t)))

;; build-lexer : syntax-object list ->
;;   (values table nat (vector-of (union #f syntax-object)) (vector-of bool) (list-of syntax-object))
;; each syntax object has the form (re action)
(define (build-lexer sos)
  (define disappeared-uses (box null))
  (define s-re-acts (for/list ([so (in-list sos)])
                              (cons (parse (stx-car so) disappeared-uses)
                                    (stx-car (stx-cdr so)))))
  (define cache (make-cache))
  (define re-acts (for/list ([s-re-act (in-list s-re-acts)])
                            (cons (->re (car s-re-act) cache)
                                  (cdr s-re-act))))
  (define dfa (build-dfa re-acts cache))
  (define table (dfa->1d-table dfa))
  ;(print-dfa dfa)
  #;(let ((num-states (vector-length table))
          (num-vectors (length (filter values (vector->list table))))
          (num-entries (apply + (map
                                 (λ (x) (if x (vector-length x) 0))
                                 (vector->list table))))
          (num-different-entries
           (let ((ht (make-hash)))
             (for-each
              (λ (x)
                (when x
                  (for-each
                   (λ (y)
                     (hash-set! ht y #t))
                   (vector->list x))))
              (vector->list table))
             (length (hash-table-map ht cons)))))
      (printf "~a states, ~aKB\n"
              num-states
              (/ (* 4.0 (+ 2 num-states (* 2 num-vectors) num-entries
                           (* 5 num-different-entries))) 1024)))
  (values table (dfa-start-state dfa) (dfa->actions dfa) (dfa->no-look dfa)
          (unbox disappeared-uses)))
 
