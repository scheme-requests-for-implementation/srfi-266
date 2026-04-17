; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026

; trick to get distinct variables, to be changed
(define (nvar)
  (let ((x (generate-temporaries '(x))))
    (car x)))

; default priority of calls
(define call-priority 45)

; the less priority
(define least-priority 1000)

; generate a procedure that return (not (symbol args ...))
(define (gen-not symbol)
  (lambda (tid args)
    `(,(datum->syntax tid 'not) (,(datum->syntax tid symbol) . ,args))))

; procedure for "? a"
(define (op-bool2int tid args)
  `(,(datum->syntax tid 'if) ,(car args) 1 0))

; procedure for "a implies b"
(define (op-implies tid args)
  `(,(datum->syntax tid 'or) (,(datum->syntax tid 'not) ,(car args)) ,(cadr args)))

; procedure for "a if b else c"
(define (op-if tid args)
  `(,(datum->syntax tid 'if) ,(cadr args) ,(car args) ,(caddr args)))

; procedure for "a as b in c"
(define (op-as tid args)
  `(,(datum->syntax tid 'let-values) ((,(cadr args) ,(car args))) ,(caddr args)))

; procedure for "a >> b"
(define (op->> tid args)
  `(,(datum->syntax tid 'arithmetic-shift) ,(car args) (,(datum->syntax tid '-) ,(cadr args))))

; standard operators
(define stdops `(
    (@         left      10  vector-ref)
    (@.        left      10  list-ref)
    (@@        left      10  bytevector-u8-ref)
    (@         prefix    10  unbox)
    (**        left      20  expt)
    (-         prefix    30  -)
    (+         prefix    30  +)
    (not       prefix    30  not)
    (?         prefix    30  ,op-bool2int)
    (*         list      40  *)
    (/         list      40  /)
    (\         left      40  quotient)
    (%         left      40  remainder)
    (+         list      50  +)
    (-         list      50  -)
    (<         comp      80  <)
    (>         comp      80  >)
    (<=        comp      80  <=)
    (>=        comp      80  >=)
    (=         comp      80  =)
    (!=        left      90  ,(gen-not '=))
    (and       list     130  and)
    (or        list     140  or)
    (implies   left     150  ,op-implies)

    (if        ternary  160  ,op-if)
    (else      ternary2 160  if)
    (as        as       160  ,op-as)
    (in        ternary2 160  as)

    (~         prefix    30  bitwise-not)
    (<<        left      60  arithmetic-shift)
    (>>        left      60  ,op->>)
    (&         list     100  bitwise-and)
    (^         list     110  bitwise-xor)
    (:         list     120  bitwise-ior)
    (~&        left     100  bitwise-nand)
    (~^        left     110  bitwise-eqv)
    (~:        left     120  bitwise-nor)

    (fx-       prefix    30  fxneg)
    (fx~       prefix    30  fxnot)
    (fx*       left      40  fx*)
    (fx\       left      40  fxquotient)
    (fx%       left      40  fxremainder)
    (fx+       left      50  fx+)
    (fx-       left      50  fx-)
    (fx<<      left      60  fxarithmetic-shift-left)
    (fx>>      left      60  fxarithmetic-shift-right)
    (fx<       comp      80  fx<?)
    (fx>       comp      80  fx>?)
    (fx<=      comp      80  fx<=?)
    (fx>=      comp      80  fx>=?)
    (fx=       comp      80  fx=?)
    (fx!=      left      90  ,(gen-not 'fx=?))
    (fx&       list     100  fxand)
    (fx^       list     110  fxxor)
    (fx:       list     120  fxior)

    (fx-       prefix    30  fl-)
    (fl*       left      40  fl*)
    (fl/       left      40  fl/)
    (fl\       left      40  flquotient)
    (fl%       left      40  flremainder)
    (fl+       left      50  fl+)
    (fl-       left      50  fl-)
    (fl<       comp      80  fl<?)
    (fl>       comp      80  fl>?)
    (fl<=      comp      80  fl<=?)
    (fl>=      comp      80  fl>=?)
    (fl=       comp      80  fl=?)
    (fl!=      left      90  ,(gen-not 'fl=?))
  ))

; extract items of operator description
(define op-symbol   car)
(define op-type     cadr)
(define op-priority caddr)
(define op-repl     cadddr)

; The context handles the informations
;  - priority
;  - function call allowed as funok
;  - operation list as opdefs
; It is an improper list
(define (make-context priority funok opdefs)
  (cons priority (cons funok opdefs)))

(define context-priority car)
(define context-funok    cadr)
(define context-opdefs   cddr)

; return the context with the given priority
(define (context-of-priority context priority)
  (cons priority (cdr context)))

; oper record the original item
; of an operator with the operator using a pair
(define make-oper cons)
(define oper-item car)
(define oper-op   cdr)

; check if the 2 operator are the same
(define (oper-eq? oper1 oper2)
  (and oper1
       oper2
       (eq? (oper-op oper1) (oper-op oper2))))

; get the context coming from applying oper
(define (oper-context oper context)
  (context-of-priority context (op-priority (oper-op oper))))

; check if the oper applies in the context
(define (oper-applies? oper context)
  (> (context-priority context) (op-priority (oper-op oper))))

; get the type of oper
(define (oper-type oper)
  (op-type (oper-op oper)))

; test if oper2 is the second operator of a ternary operation
; started by oper1
(define (oper-ternary-fits? oper1 oper2)
  (let ((op1 (oper-op oper1))
        (op2 (oper-op oper2)))
    (and (eqv? (op-type op2) 'ternary2)
         (eqv? (op-repl op2) (op-symbol op1)))))

; transform the operator 'oper' on its arguments
(define (oper-apply oper args)
  (let ((tid  (oper-item oper))
	(repl (op-repl (oper-op oper))))
    (cond
      ((symbol? repl)
        (cons (datum->syntax tid repl) args))
      ((procedure? repl)
        (repl tid args))
      (else
        (error "bad operator definition" (oper-op oper))))))

; get the prefix operation of given item
; or #f if item doesn't stand for a prefix
(define (s-oper item context prefix?)
  (and (identifier? item)
       (let ((sym (syntax->datum item)))
	 (let lp ((iter-opdefs (context-opdefs context)))
	   (and (pair? iter-opdefs)
		(let ((opdef (car iter-opdefs)))
		  (if (and (eqv? sym (op-symbol opdef))
                           (eqv? prefix? (eqv? 'prefix (op-type opdef))))
                    (make-oper item opdef)
		    (lp (cdr iter-opdefs)))))))))

; get the prefix operation of given item
; or #f if item doesn't stand for a prefix
(define (s-prefix item context)
  (s-oper item context #t))

; get the operation that is not a prefix of given item
; or #f if item doesn't stand for a not prefix
(define (s-infix item context)
  (s-oper item context #f))

; first item of expression or #f
(define (t-first expression)
  (and (pair? expression) (car expression)))

; rest of expression after removing first item or #f
(define (t-rest expression)
  (and (pair? expression)
       (let ((rest (cdr expression)))
         (and (pair? rest) rest))))

; get value of the item
; an item is either:
;  - an atom (symbol, number, boolean)
;    in that case, the item value is the atom
;  - a list containing only a list as in ((...))
;    in taht case, the item value is the expression as is
;  - a list
;    in that case expre is evaluated for it
(define (t-term item context)
  (syntax-case item ()
    (((lst ...))  (syntax (lst ...)))
    ((lst ...)    (syntax (expr lst ...)))
    (_            item)))

; get the transformed expression of the head of 'expression'
; the tansformation of expressions of priority lower than 'priority'
; call the continuation 'cont' with the replaced value of the
; first term and the remaining part
(define (t-prefix expression context cont)
  ; split in head and tail and search if head is a prefix operator
  (let* ((head (t-first expression))
         (rest (t-rest expression))
         (oper (s-prefix head context)))
    (if oper
      ; head is a prefix operator, get the term where it apply
      (t-prefix rest (oper-context oper context)
        (lambda (term rest)
          (t-cont (oper-apply oper (list term)) rest context cont)))
      ; head isn't a prefix operator so it is a term
      (t-cont (t-term head context) rest context cont))))

; get the transformation of 'term' followed by 'rest'
; searches for an operator at begin of rest
; when funok isn't false, function call is allowed
(define (t-cont term rest context cont)
  ; get head of rest
  (let ((head (and rest (t-first rest))))
    (if (not head)
      ; no rest
      (cont term rest)
      ; check if head of rest is an operator
      (let ((oper (s-infix head context)))
        (if oper
          ; operator found, check its priority
          (if (oper-applies? oper context)
            (t-infix term oper (t-rest rest) context cont)
            (cont term rest))
          ; no operator check if call allowed
          (if (context-funok context)
            (t-call term rest context cont)
            (cont term rest)))))))

; transform a function call
; func is the expression representing the procedure to be called
(define (t-call func rest context cont)
  (let ((head (t-first rest)))
    ; check if head is a list
    (syntax-case head ()
      ((args ...)
        ; it is a pair, space are argument's separation not function calls
        (let ((arg-ctxt (make-context least-priority #f (context-opdefs context))))
          ; loop on args
          (let t-arg ((rlst (list func))
                      (frst (syntax (args ...))))
            (t-prefix frst arg-ctxt
              (lambda (term frst)
                (if frst
                  ; not at end, should be an other argument
                  (t-arg (cons term rlst) frst)
                  ; end of arguments
                  (t-cont (reverse (cons term rlst)) (t-rest rest) context cont)))))))
      (_
        ; only one argument follows
        (t-prefix rest (context-of-priority context call-priority)
          (lambda (term rest)
            (t-cont (list func term) rest context cont)))))))

; transform an infix operator 'oper' preceded by 'term' and followed
; by 'rest'
(define (t-infix term oper rest context cont)
  ; detect empty rest
  (unless rest
    (syntax-violation #f "unexpected end after operator" (oper-item oper)))
  ; prepare the ccontext of the operation
  (let ((op-ctxt (oper-context oper context)))
    ; processing depends of the type of the operator
    (case (oper-type oper)

      ; for left associative operators
      ((left)
        (t-prefix rest op-ctxt
                (lambda (second rest)
                  (let* ((args (list term second))
                         (item (oper-apply oper args)))
                    (t-cont item rest context cont)))))

      ; for operators whose operand can go in list (*, +, -, /)
      ((list)
        ; get the list of terms that have the same operator
        (let lp ((rlst (list term))
                 (rest rest))
          ; scan the first term of rest
          (t-prefix rest op-ctxt
                  (lambda (term rest)
                    ; scan head of rest for the same operator
                    (let* ((next  (t-first rest))
                           (nope  (s-infix next context)))
                      (if (oper-eq? nope oper)
                        ; it is the same, add the term to list and loop
                        (lp (cons term rlst) (t-rest rest))
                        ; not the same, continue
                        (let* ((args (reverse (cons term rlst)))
                               (item (oper-apply oper args)))
                          (t-cont item rest context cont))))))))

      ; for comparison operators, it translates a < b <= c in (let ((x b)) (and (< a x) (<= x c)))
      ((comp)
        ; get the list of terms that have the same operator
        (let lp ((rlst (list term))
                 (rest rest))
          ; scan the first term of rest
          (t-prefix rest op-ctxt
                  (lambda (term rest)
                    ; scan head of rest for the same operator
                    (let* ((next  (t-first rest))
                           (nope  (s-infix next context)))
                      (if (oper-eq? nope oper)
                        ; it is the same, add the term to list and loop
                        (lp (cons term rlst) (t-rest rest))
                        ; it is not the same, check if it is of type comp
                        (if (not (and nope (eqv? 'comp (oper-type nope))))
                          ; not of type comp, continue
                          (let* ((args (reverse (cons term rlst)))
                                 (item (oper-apply oper args)))
                            (t-cont item rest context cont))
                          ; it is of type comp
                          ; create a variable for holding current 'term' value
                          ; create the comparison of current operator with this variable
                          (let* ((var   (nvar))
                                 (args  (reverse (cons var rlst)))
                                 (comp1 (oper-apply oper args)))
                            ; scan the new comparison
                            (t-infix var nope (t-rest rest) context
                              (lambda (comp2 rest)
                                ; produce the result and continue
                                (let ((item `(,(datum->syntax term 'let) ((,var ,term)) (,(datum->syntax term 'and) ,comp1 ,comp2))))
                                  (t-cont item rest context cont))))))))))))

      ; ternary as 3 parts, the third one is separated by the matching ternary2
      ((ternary)
        (letrec ((t-middle
                  (lambda (term2 rest)
                    ; scan head of rest for the second ternary operator
                    (let* ((head  (t-first rest))
                           (nope  (s-infix head context)))
                      ; test if head is the second symbol of ternary
                      (if (not nope)
                        (syntax-violation #f "ternary not closed" (oper-item oper))
                        (if (not (oper-ternary-fits? oper nope))
			  ; no, so maybe a nested ternary
                          (t-infix term2 nope (t-rest rest) op-ctxt t-middle)
			  ; yes, get third expression and conclude
                          (t-prefix (t-rest rest) op-ctxt
                                (lambda (term3 rest)
                                  (let* ((args (list term term2 term3))
                                         (item (oper-apply oper args)))
                                    (t-cont item rest context cont))))))))))
          ; scan the second expression of the ternary
          (t-prefix rest op-ctxt t-middle)))

      ; as is special, the second part is a list of symbols
      ((as)
        ; get second expression, a list of items until ternary fit
        (let t-id ((term2 '())
                   (rest  rest))
	  ; scan head
          (let ((head  (t-first rest))
                (rest  (t-rest rest)))
	    ; test if head is the second symbol of the ternary
            (if (not head)
              (syntax-violation #f "as not closed" (oper-item oper))
              (let ((nope  (s-infix head context)))
                (if (and nope (oper-ternary-fits? oper nope))
		  ; yes, get third expression and conclude
                  (t-prefix rest op-ctxt
                        (lambda (term3 rest)
                          (let* ((args (list term (reverse term2) term3))
                                 (item (oper-apply oper args)))
                            (t-cont item rest context cont))))
		  ; no, capture head in term2 and iterate
                  (t-id (cons head term2) rest)))))))

      ((right)
        (error "not yet implemented" (oper-item oper)))

      ((ternary2)
        (syntax-violation #f "second of ternary without first" (oper-item oper)))

      (else
        (error "unexpected operator type" (oper-type oper) (oper-item oper) rest)))))

; root function for processing expression accordingly to operator definitiions
(define (t-expr expression opdefs)
  (t-prefix expression (make-context least-priority #t opdefs)
    (lambda (result rest)
      (when rest
        (syntax-violation #f "remaining part" rest))
      result)))

; meta syntax definition for defining a syntax doing expr processing
; accordingly to operator definitions
(define-syntax define-expr-syntax
  (syntax-rules etc ()
    ((_ name opdefs)
      (define-syntax name
        (lambda (x)
          (syntax-case x ()
            ((_ term ...)
              (t-expr (syntax (term ...)) opdefs))))))))

; definition of expr using define-expr-syntax and standard operators' definition
(define-expr-syntax expr stdops)

; syntax for defining lambda simply evaluating expr
(define-syntax lambda-expr
  (syntax-rules ()
    ((_ args terms ...)
      (lambda args (expr terms ...)))))

; syntax for defining procedure simply evaluating expr
(define-syntax define-expr
  (syntax-rules ()
    ((_ (name . args) terms ...)
      (define (name . args) (expr terms ...)))))


