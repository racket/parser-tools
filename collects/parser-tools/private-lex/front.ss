(module front mzscheme
  (require "util.ss"
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
  
  
  ;; dfa->table : dfa -> (same as build-lexer)
  (define (dfa->table dfa)
    (let (
          ;; no-look : (vector-of bool)
          ;; For each state whether the lexer can ignore the next input.
          ;; It can do this only if there are no transitions out of the
          ;; current state.
          (no-look (make-vector (dfa-num-states dfa) #t))
          
          ;; actions : (vector-of (union #f syntax-object))
          ;; The action for each final state, #f if the state isn't final
          (actions (make-vector (dfa-num-states dfa) #f))
          
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
                         (char-set-for-each (lambda (char)
                                              (vector-set! char-table
                                                           (bitwise-ior 
                                                            char
                                                            (arithmetic-shift from-state 8))
                                                           to-state))
                                            (car chars/to))))
                     (cdr trans))))
       (dfa-transitions dfa))
      
      (for-each (lambda (trans)
                  (vector-set! no-look (car trans) #f))
                (dfa-transitions dfa))

      (for-each (lambda (state/action)
                  (vector-set! actions (car state/action) (cdr state/action)))
                (dfa-final-states/actions dfa))
      
      (values char-table (dfa-start-state dfa) actions no-look)))
    
  (test-block ()
              ((call-with-values (lambda ()
                                   (dfa->table (make-dfa 1 1 (list) (list))))
                                 list)
               (list (make-vector 256 #f) 1 (vector #f) (make-vector 1 #t)))
              ((call-with-values (lambda ()
                                   (dfa->table (make-dfa 4 1 (list (cons 2 2) (cons 3 3))
                                                         (list (cons 1 (list (cons (make-range 49 50) 1)
                                                                             (cons (make-range 51 51) 2)))
                                                               (cons 2 (list (cons (make-range 49 49) 3)))))))
                                 list)
               (list (let ((v (make-vector 1024 #f)))
                       (vector-set! v 305 1)
                       (vector-set! v 306 1)
                       (vector-set! v 307 2)
                       (vector-set! v 561 3)
                       v)
                     1
                     (vector #f #f 2 3)
                     (vector #t #f #f #t))))
  
  ;; build-lexer : syntax-object list -> (values (vector-of (union #f nat)) nat (vector-of (union #f syntax-object)) (vector-of bool))
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
      (dfa->table dfa)))
  )
