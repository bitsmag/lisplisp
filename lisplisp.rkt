#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;L..I..S..P..L..I..S..P;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;basic loop
(define (REPL)
  (display ">")
  (display (EVAL (read) ENVIRONMENT))
  (display "\n")
  (REPL))

;;;create a new, empty environment
(define (newEnv)
  (mcons null null))

;;;define ENVIRONMENT as the new, empty environment
(define ENVIRONMENT (newEnv))

;;;add a pair (symbol/value) to an environment
(define (add-binding-helper envList key value)
  (cons
   (cons key value)
   envList))
(define (add-binding env key value)
  (set-mcdr!
   env
   (add-binding-helper (mcdr env) key value)))

;;;read the value from a symbol in an environment
(define (get-binding-helper envList key)
  (if (eq? envList null)
      (error "no such binding")
      (let ((firstBinding (car envList))
            (restBinding (cdr envList)))
      (if (eq? (car firstBinding) key)
          (cdr firstBinding)
          (get-binding-helper restBinding key)))))
(define (get-binding env key)
  (get-binding-helper (mcdr env) key))

;;;evaluate something in an environment (note capital letters to differentiate between EVAL and eval (native 'eval'-funtion))
(define (EVAL e env)
  (if (number? e)
      e
  (if (symbol? e)
      (get-binding env e)
  (if (pair? e)
      (EVALFunctionCall e env)
      (error "not implemented")))))

;;;map (note capital letters to differentiate between MAP and map (native 'map'-funtion))
(define (MAP f l)
  (if (eq? l null)
      l
      (cons (f (car l)) (MAP f (cdr l)))))

;;;evalualte a list of arguments
(define (EVALArgList l env)
  (MAP (lambda (a) (EVAL a env)) l))

;;;evalualte function calls
(define (EVALFunctionCall e env)
  (let ((fn (car e)))
        (if (eq? fn 'if)
            (EVALIf (EVALArgList (cdr e) env))
        (if (eq? fn 'define)
            (EVALDefine (cdr e) env)
        (if (eq? fn 'lambda)
            (EVALLambda (EVALArgList (cdr e) env))
        (if (eq? fn 'set!)
            (EVALSet (EVALArgList (cdr e) env))
        (if (eq? fn '+)
            (EVALPlus (EVALArgList (cdr e) env))
        (if (eq? fn 'eq?)
            (EVALEqual (EVALArgList (cdr e) env))
            (EVALUserDefined e)))))))))

;;;implementation of build in functions
(define (EVALIf argList)
    (let ((cond (car argList))
          (ifPart (car (cdr argList)))
          (elsePart (car (cdr (cdr argList)))))
    (if cond ifPart elsePart)))

(define (EVALDefine argList env)
    (add-binding ENVIRONMENT (car argList) (EVAL (car (cdr argList)) env)))

(define (EVALLambda argList)
    "not implemented")

(define (EVALSet argList)
    "not implemented")

(define (EVALPlus argList)
  (+ (car argList) (car (cdr argList))))

(define (EVALEqual argList)
  (eq? (car argList) (car (cdr argList))))

(define (EVALUserDefined argList)
    "not implemented")
