
#lang racket

(provide (all-defined-out))

(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

(define empty-state (make-hash))


(define (get state var)
  (if (hash-has-key? state var)
      (hash-ref state var)
      (eval var)))

(define (put state var val)
  (hash-set state var val))



(define (:= var val-expr state)
  (let* ((new-state (eval-expr val-expr state))
        (result (hash-ref new-state '-r))
        (new-var-hash (create-hash (list var '-r) (list result result)))
        )
 
    (if (hash-empty? state)
        new-var-hash
        (add-to-hash state new-var-hash)
    )
  ) 
)


(define (if: test-expr then-expr else-expr state)
  (let ((result_state (eval-expr test-expr state))
        )
    
     
      (if (hash-ref result_state '-r)
                (eval-exprs then-expr state)
                (eval-exprs else-expr state)
      ))
)


(define (while: test-expr body-expr state)
  (let ((result-state (eval-expr test-expr state)))
    
 
  (if (hash-ref result-state '-r)
      (let ((new-state (eval-exprs  body-expr state)))
      
        (while: test-expr body-expr new-state)
        )
      result-state
      
      )

  ))



(define (map-eval lst state)
  (let loop ((lst lst) (state state) (results '()))
    (cond
      ((null? lst) (cons (reverse results) state))
      ((pair? (car lst))
       (let* ((nested-results (map-eval (car lst) state))
              (nested-state (car (reverse nested-results)))
              (nested-results (reverse (cdr (reverse nested-results)))))
         (loop (cdr lst) nested-state (append results nested-results))))
      (else
       (let* ((result (eval-expr (car lst) state))
              (new-state (if (hash-empty? state)
                             result
                             (add-to-hash state result))))
         (loop (cdr lst) new-state (cons (hash-ref result '-r) results)))))))








(define (func params body-exprs state)
  (let (
    
    (... (lambda (args . kwargs)
                   
                    (let* (
                          (lst (cons args kwargs))
                          (new-state (create-hash  params lst ))
                          (newer-state (if ( hash-empty? state)
                                            new-state
                                            (
                                        add-to-hash state new-state )))
                          (result-state (eval-exprs body-exprs newer-state))
                          (result (hash-ref result-state '-r))
                          )
                      result)
           
                )
              )
    
    )
    (if (hash-empty? state)
        (create-hash (list '-r) (list ...))
        (add-to-hash state (create-hash (list '-r) (list ...)))
    )
    )
)




(define (eval-expr expr state)
  (cond
    ((list? expr)
   
     (let* ((operation (car expr))
           (args (cdr expr)))
    
       
       (cond
         
         
         ( (eq? operation ':=)
          (let* ((var (car args))
                (val-expr (cadr args))
                (new_state (:= var val-expr state))
                )
          
            (if (hash-empty? state)
                new_state
                (add-to-hash state new_state)
             )
            )
            )

         ((eq? operation 'if:)
           (let* ((test-expr (car args))
                (then-expr (cadr args))
                (else-expr (caddr args))
                (result (if: test-expr then-expr else-expr state))
                )
           (if (hash-empty? state)
               result
               (add-to-hash state result)
               )
           )
          
          )

         ((eq? operation 'while:)
          (let* ((test-expr (car args))
                     (body-expr (cadr args))
                     (result (while: test-expr body-expr state))
                     )
                (if (hash-empty? state)
                    result
                    (add-to-hash state result)
                    )
                )

          )

         ((eq? operation 'func)
          (let* ((params (car args))
                     (body-exprs (cdr args))
                     (result (func params body-exprs state))
                     )
               
                (if (hash-empty? state)
                    result
                    (add-to-hash state result)
                    )
                )
         )
          ((eq? operation 'lambda)
            (let* ((params (car args))
                     (body-exprs (cdr args))
                     (result (func params body-exprs state))
                     )
                (if (hash-empty? state)
                    result
                    (add-to-hash state result)
                    )
                )
         )
         (else
          
         
           (let* ((args-states (map (lambda (e) (eval-expr e state)) args))
                  (args-values (map (lambda (s) (hash-ref s '-r)) args-states))
                  (operator (eval (car expr)))
 
                 (result (apply operator args-values))
                  (new-state (create-hash (list '-r) (list result))))
             
             (if (hash-empty? state)
                 new-state
                 (add-to-hash state new-state))
             )))
         
         ))
    
    ((symbol? expr)
   
     
     (if (hash-has-key? state expr)
         (hash-set state '-r (hash-ref state expr))
         (hash-set state '-r (eval expr))
         )
     )
    
    
    (else
     (if (hash-empty? state)
         
            (create-hash (list '-r) (list expr))
            (hash-set state '-r (eval expr))
        )
     
     )))


(define (eval-exprs exprs state)
  (let ((exprs (if (and (list? (car exprs)) (list? (car (car exprs))))
                   (car exprs)
                   exprs
                   ))) 
    
    (foldl (lambda (expr state) (eval-expr expr state))
           state
           exprs))
)
  