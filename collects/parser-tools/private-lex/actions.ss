(module actions mzscheme

  (provide (all-defined))
  
  ;; wrap-action: (syntax-object or #f) symbol syntax-object syntax-object -> syntax-object
  (define (wrap-action action result-name ctxt loc)
    (if action
        (let ((parms (datum->syntax-object
                      action
                      `(start-pos end-pos ,result-name return-without-pos input-port))))
          (datum->syntax-object ctxt
                                `(lambda ,parms ,action)
                                loc))
        (datum->syntax-object ctxt 'void loc)))
        
    
  ;; get-special-action: (syntax-object list) symbol 'a -> syntax-object or 'a
  ;; Returns the first action from a rule of the form ((which-special) action)
  (define (get-special-action rules which-special none)
    (cond
      ((null? rules) none)
      (else
       (syntax-case (car rules) ()
         (((special) act)
          (eq? (syntax-e (syntax special)) which-special)
          (syntax act))
         (_ (get-special-action (cdr rules) which-special none))))))
  
  ;; filter-out-specials: (syntax-object list) (symbol list) -> (syntax-object list)
  ;; Returns a list missing all the rules of the form ((special) action)
  ;; where special is a symbol in which specials.
  (define (filter-out-specials rules which-specials)
    (cond
      ((null? rules) null)
      (else
       (syntax-case (car rules) ()
         (((special) act)
          (memq (syntax-e (syntax special)) which-specials)
          (filter-out-specials (cdr rules) which-specials))
         (_ (cons (car rules) (filter-out-specials (cdr rules) which-specials)))))))
  
  )