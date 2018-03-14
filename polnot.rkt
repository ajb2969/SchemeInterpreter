; The Polnot language interpreter
; Alex Brown
; Here is a sample program to test the interpreter.
;
(define prog1
  '(
    (:= a 5)
    (:= b + + a 2 * a 10)
    (:= c / - b 5 4)
    (print a)
    (print b)
    (print c)
    (:= a + 660 - c 3)
    (print a)
    (print b)
    (print c)
    (print + a + b c)
    (print + 1 d)
    (print + 1 & 5 6)
    )
  )

(define prog2
  '(
    (:= a 5)
    (print a)
    (:= b + + a 2 * a 10)
    (print b)
    )
  )

;Name:getVal
;Param: key - the key to be looked for
;Param: env - the environment used to search for the key
;Return: the value of the key if it exists
(define getVal(lambda (key env)
                 (cond
                       ((null? env) 999);end of list - not in list
                       ((equal? (car (car env)) key) (car (cdr (car env))))
                       ;if the key exists - return #t
                       (#t (getVal key (cdr env) ));recurse on the list
                 )
               ))



;Name: makeNode
;Param: varName - the name of the symbol
;Param: val - the value to be given to the symbol
;Param: env - the environment used in the symbol's value's evaluation
;Return: a list containing the varName and the evaluated val
(define makeNode (lambda (varName val env)
                    (cons (cons varName (evaluate val env)) env)
                    ))

;Name: execute
;Param: cmd - the command to be executed
;Param: env - the environment to be used during cmd's execution
(define execute (lambda(cmd env)
                  (cond
                    ((equal? (car cmd) ':=)  (makeNode (cadr cmd) (cddr cmd) env))
                    ((equal? (car cmd) 'print)
                     (display (car (evaluate (cdr cmd) env))) (newline) env)
                  )
))

;Name:initEnv
;Param:programList - the list of operations to be performed
;Param:env - the environment to be built and used
(define initEnv (lambda (programList env)
                (cond
                ((null? programList) env)
                (#t (initEnv (cdr programList) (execute (car programList) env)))
               )
))

;Name: run
;Param: pnprog - a list of lists containing program operations
(define run (lambda (pnprog);
              (initEnv pnprog '())
 ))

;Name:performOperation
;Param:op - the operation to be performed
;Param:oper1 - the lhs of the expression
;Param:oper2 - the rhs of the expression
;Return:result of oper1 op oper2
(define performOperation(lambda (op oper1 oper2)
                          (cond
                            ((equal? op '+) (list (+ oper1 oper2 )))
                            ((equal? op '-) (list (- oper1 oper2 )))
                            ((equal? op '*) (list (* oper1 oper2 )))
                            ((equal? op '/) (list (/ oper1 oper2 )))
                          )
          ))

;Name:isOperation
;Param:op - the operatation to be tested
;Return: a boolean value is the op param is an operator
(define isOperation (lambda (op)
                      (cond
                            ((equal? op '+) #t)
                            ((equal? op '-) #t)
                            ((equal? op '*) #t)
                            ((equal? op '/) #t)
                            (#t #f)
                       )

                      ))

;Name: stringTogether
;Param: expr - an expression stored in a list
;Param: env - the environment
(define stringTogether (lambda (expr env)
             (cond
             ((number? (car expr)) expr);if it's a number
             ((isOperation (car expr))
              (let
                  (
                    (leftSide (stringTogether (cdr expr) env))
                    (rightSide (stringTogether
                                (cdr (stringTogether (cdr expr) env)) env))
                    (oper (car expr))
                  )
                (append (performOperation oper (car leftSide) (car rightSide))
                        (cdr rightSide))
                  
              )
              )
             (#t (cons (getVal (car expr) env) (cdr expr)));if it's a symbol
                 )))

;Name: evaluate
;Param: expr - an expression stored in a list
;Param: env - the environment
(define evaluate (lambda (expr env);create parse tree to evaluate prefix expression
            (cond
             ((number? (car expr))  (cons (car expr) (cdr expr)));if it's a number
             (#t (stringTogether expr env))
             )
))



; Some suggested test expressions
; (define final-env (run prog1))
; final-env ; Structure of environment is unspecified.
; (evaluate '(a) final-env)
; (evaluate '(- - a b c) final-env)

