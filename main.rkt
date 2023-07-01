; Batuhan Solmaz
; 2021400147
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; read and parse the input file
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

; solution starts here
; 1. empty-state (5 points)
(define empty-state (make-hash))

; 2. get (5 points)
;(define get 0)
(define (get state var)
  (if (hash-has-key? state var)
      (hash-ref state var)
      (eval var)))

; 3. put (5 points)
(define (put state var val)
  (hash-set state var val))

; 4. := (15 points)
;(define := 0)


(define (:= var val-expr state)
  (let* ((new-state (eval-expr val-expr state))
        (result (hash-ref new-state '-r))
        (new-var-hash (create-hash (list var '-r) (list result result)))
        )
    ;add the new value to the hash and -r value to the hash
   ;(display new-var-hash)
    ;(display val-expr)
    ;(display new-var-hash)
    (if (hash-empty? state)
        new-var-hash
        (add-to-hash state new-var-hash)
    )
  ) 
)

; 5. if: (15 points)
(define (if: test-expr then-expr else-expr state)
  (let ((result_state (eval-expr test-expr state))
        )
    
      ;(display result_state)
      (if (hash-ref result_state '-r)
                (eval-exprs then-expr state)
                (eval-exprs else-expr state)
      ))
)



; 6. while: (15 points)
(define (while: test-expr body-expr state)
  (let ((result-state (eval-expr test-expr state)))
    
  ;(display result-state)  
  (if (hash-ref result-state '-r)
      (let ((new-state (eval-exprs  body-expr state)))
        ;(display (car body-expr))
        ;(display new-state)
        (while: test-expr body-expr new-state)
        )
      result-state
      
      )

  ))


; 7. func (15 points)
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
                    ;match the params with the args
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
     ;(display 1)
     (let* ((operation (car expr))
           (args (cdr expr)))
       ;(display operation)
       ;((list? operation) (eval-expr operation state)  
       
       (cond
         
         
         ( (eq? operation ':=)
          (let* ((var (car args))
                (val-expr (cadr args))
                (new_state (:= var val-expr state))
                )
            ;(display state)
            ;(display new_state)
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
                 ;(display body-exprs)
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
          
          ;(display expr)
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
     ;(display 2)
     
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


; 9 eval-exprs (5 points)
(define (eval-exprs exprs state)
  (let ((exprs (if (and (list? (car exprs)) (list? (car (car exprs))))
                   (car exprs)
                   exprs
                   ))) 
    ;(display exprs)
    (foldl (lambda (expr state) (eval-expr expr state))
           state
           exprs))
)
  