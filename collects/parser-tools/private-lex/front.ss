(module front mzscheme
  (require (prefix is: (lib "integer-set.ss"))
           (lib "list.ss")
           "util.ss"
           "stx.ss"
           "re.ss"
           "deriv.ss")
  
  (provide build-lexer)
  
  (define-syntax time-label
    (syntax-rules ()
      ((_ l e ...)
       (begin
         (printf "~a: " l)
         (time (begin e ...))))))
  
  ;; A table is either
  ;; - (vector-of (union #f nat))
  ;; - (vector-of (vector-of (cons (cons nat nat) nat)))
  
  (define loc:integer-set-contents is:integer-set-contents)
  
  ;; dfa->1d-table : dfa -> (same as build-lexer)
 (define (dfa->1d-table dfa)
   (let ((state-table (make-vector (dfa-num-states dfa) #f)))
     (for-each (lambda (trans)
                 (let* ((from-state (car trans))
                        (all-chars/to (cdr trans))
                        (flat-all-chars/to
                         (mergesort
                          (apply append
                                 (map (lambda (chars/to)
                                        (let ((char-ranges (loc:integer-set-contents (car chars/to)))
                                              (to (cdr chars/to)))
                                          (map (lambda (char-range)
                                                 (cons char-range to))
                                               char-ranges)))
                                      all-chars/to))
                          (lambda (a b)
                            (< (caar a) (caar b))))))
                   (vector-set! state-table from-state (list->vector flat-all-chars/to))))
               (dfa-transitions dfa))
     state-table))
  
         
  (define loc:foldr is:foldr)
  
  ;; dfa->2d-table : dfa -> (same as build-lexer)
  (define (dfa->2d-table dfa)
    (let (
          ;; char-table : (vector-of (union #f nat))
          ;; The lexer table, one entry per state per char.
          ;; Each entry specifies a state to transition to.
          ;; #f indicates no transition
          (char-table (make-vector (* 256 (dfa-num-states dfa)) #f)))
      
      ;; Fill the char-table vector
      (for-each 
       (lambda (trans)
         (let ((from-state (car trans)))
           (for-each (lambda (chars/to)
                       (let ((to-state (cdr chars/to)))
                         (loc:foldr (lambda (char _)
                                     (vector-set! char-table
                                                  (bitwise-ior 
                                                   char
                                                   (arithmetic-shift from-state 8))
                                                  to-state))
                                   (void)
                                   (car chars/to))))
                     (cdr trans))))
       (dfa-transitions dfa))
      char-table))
      

  ;; dfa->actions : dfa -> (vector-of (union #f syntax-object))
  ;; The action for each final state, #f if the state isn't final
  (define (dfa->actions dfa)
    (let ((actions (make-vector (dfa-num-states dfa) #f)))
      (for-each (lambda (state/action)
                  (vector-set! actions (car state/action) (cdr state/action)))
                (dfa-final-states/actions dfa))
      actions))
      
  ;; dfa->no-look : dfa -> (vector-of bool)
  ;; For each state whether the lexer can ignore the next input.
  ;; It can do this only if there are no transitions out of the
  ;; current state.
  (define (dfa->no-look dfa)
    (let ((no-look (make-vector (dfa-num-states dfa) #t)))
      (for-each (lambda (trans)
                  (vector-set! no-look (car trans) #f))
                (dfa-transitions dfa))
      no-look))
      
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
                                  #(((49 . 50) . 1) ((51 . 51) . 2))
                                  #(((49 . 49) . 3))
                                  #f))
              ((dfa->1d-table d3) #(#f
                                  #(((49 . 50) . 1) ((51 . 51) . 2) ((100 . 200) . 0))
                                  #(((49 . 49) . 3))
                                  #f))
              ((dfa->actions d1) (vector #f))
              ((dfa->actions d2) (vector #f #f 2 3))
              ((dfa->no-look d1) (vector #t))
              ((dfa->no-look d2) (vector #t #f #f #t)))

  ;; build-lexer : syntax-object list -> (values table nat (vector-of (union #f syntax-object)) (vector-of bool))
  ;; each syntax object has the form (re action)
  (define (build-lexer sos)
    (let* ((s-re-acts (map (lambda (so)
                             (cons (parse (car (syntax->list so)))
                                   (cadr (syntax->list so))))
                           sos))

           (cache (make-cache))
           
           (re-acts (map (lambda (s-re-act)
			   (cons (->re (car s-re-act) cache)
				 (cdr s-re-act)))
			 s-re-acts))
           
           (dfa (build-dfa re-acts cache)))
      ;(print-dfa dfa)
      ;(printf "states: ~a~n" (dfa-num-states dfa))
      (values (dfa->1d-table dfa) (dfa-start-state dfa) (dfa->actions dfa) (dfa->no-look dfa))))
  )
