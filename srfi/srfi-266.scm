; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026
;
; Implementation only for Guile3

(define-module (srfi srfi-266)
  #:export
    (expr
     @ @. @@ ** ? // % !=
     implies as in ~ << >> & ^ : ~& ~^ ~:
     fx~ fx// fx% fx<< fx>> fx< fx> fx<= fx>=
     fx= fx!= fx& fx^ fx:
     fl// fl< fl> fl<= fl>= fl= fl!=))

(import
  (srfi srfi-11)
  (srfi srfi-111)
  (rnrs arithmetic fixnums (6))
  (rnrs arithmetic flonums (6))
  (rnrs arithmetic bitwise (6)))

;-------------------------------------------------------
; expr define operators that must not be confused with
; their matching operation. Then it is an error to try
; using it outside 'expr' syntax.
;-------------------------------------------------------

(define-syntax strict-operators
  (syntax-rules ::: ()
    ((_ operator :::)
       (begin
          (define-syntax operator
            (syntax-rules ()
              ((_ ...)    (syntax-error "invalid outside expr syntax" 'operator)))) :::
  ))))

(strict-operators
  @ @. @@ ** ? // % != implies as in ~ << >> & ^ : ~& ~^ ~:
  fx~ fx// fx% fx<< fx>> fx< fx> fx<= fx>= fx= fx!= fx& fx^ fx:
  fl// fl< fl> fl<= fl>= fl= fl!=)

;-------------------------------------------------------
; definition of syntax for specific operators
;-------------------------------------------------------

; implementation of operator "? x"
(define-syntax expr-?
  (syntax-rules ()
    ((_ x) (if x 1 0))))

; implementation of operator "x implies y"
(define-syntax expr-implies
  (syntax-rules ()
    ((_ x y) (or (not x) y))))

; implementation of operator "x if y else z"
(define-syntax expr-if
  (syntax-rules ()
    ((_ x y z) (if y x z))))

; implementation of operator "x as y in z"
(define-syntax expr-as
  (syntax-rules ()
    ((_ x y z) (let-values ((y x)) z))))

; implementation of operator "x != y"
(define-syntax expr-!=
  (syntax-rules ()
    ((_ x y z ...) (not (= x y z ...)))))

; implementation of operator "x fx!= y"
(define-syntax expr-fx!=
  (syntax-rules ()
    ((_ x y z ...) (not (fx=? x y z ...)))))

; implementation of operator "x fl!= y"
(define-syntax expr-fl!=
  (syntax-rules ()
    ((_ x y z ...) (not (fl=? x y z ...)))))

;-------------------------------------------------------
; expr syntax
;-------------------------------------------------------

(define-syntax expr

  (let ()

    ;---------------------------------------------------
    ; definition of standard operators
    ;---------------------------------------------------
    (define opdefs `(
        (,#'@         left      10  ,#'vector-ref)
        (,#'@.        left      10  ,#'list-ref)
        (,#'@@        left      10  ,#'bytevector-u8-ref)
        (,#'@         prefix    10  ,#'unbox)
        (,#'**        left      20  ,#'expt)
        (,#'-         prefix    30  ,#'-)
        (,#'+         prefix    30  ,#'+)
        (,#'not       prefix    30  ,#'not)
        (,#'?         prefix    30  ,#'expr-?)
        (,#'*         list      40  ,#'*)
        (,#'/         list      40  ,#'/)
        (,#'//        left      40  ,#'quotient)
        (,#'%         left      40  ,#'remainder)
        (,#'+         list      50  ,#'+)
        (,#'-         list      50  ,#'-)
        (,#'<         compare   80  ,#'<)
        (,#'>         compare   80  ,#'>)
        (,#'<=        compare   80  ,#'<=)
        (,#'>=        compare   80  ,#'>=)
        (,#'=         compare   80  ,#'=)
        (,#'!=        left      90  ,#'expr-!=)
        (,#'and       list     130  ,#'and)
        (,#'or        list     140  ,#'or)
        (,#'implies   left     150  ,#'expr-implies)

        (,#'if        ternary  160  ,#'expr-if)
        (,#'else      pair-of  160  ,#'if)
        (,#'as        as       160  ,#'expr-as)
        (,#'in        pair-of  160  ,#'as)

        (,#'~         prefix    30  ,#'bitwise-not)
        (,#'<<        left      60  ,#'bitwise-arithmetic-shift-left)
        (,#'>>        left      60  ,#'bitwise-arithmetic-shift-right)
        (,#'&         list     100  ,#'bitwise-and)
        (,#'^         list     110  ,#'bitwise-xor)
        (,#':         list     120  ,#'bitwise-ior)
        (,#'~&        left     100  ,#'bitwise-nand)
        (,#'~^        left     110  ,#'bitwise-eqv)
        (,#'~:        left     120  ,#'bitwise-nor)

        (,#'fx-       prefix    30  ,#'fxneg)
        (,#'fx~       prefix    30  ,#'fxnot)
        (,#'fx*       left      40  ,#'fx*)
        (,#'fx//      left      40  ,#'fxquotient)
        (,#'fx%       left      40  ,#'fxremainder)
        (,#'fx+       left      50  ,#'fx+)
        (,#'fx-       left      50  ,#'fx-)
        (,#'fx<<      left      60  ,#'fxarithmetic-shift-left)
        (,#'fx>>      left      60  ,#'fxarithmetic-shift-right)
        (,#'fx<       compare   80  ,#'fx<?)
        (,#'fx>       compare   80  ,#'fx>?)
        (,#'fx<=      compare   80  ,#'fx<=?)
        (,#'fx>=      compare   80  ,#'fx>=?)
        (,#'fx=       compare   80  ,#'fx=?)
        (,#'fx!=      left      90  ,#'expr-fx!=)
        (,#'fx&       list     100  ,#'fxand)
        (,#'fx^       list     110  ,#'fxxor)
        (,#'fx:       list     120  ,#'fxior)

        (,#'fl-       prefix    30  ,#'fl-)
        (,#'fl*       left      40  ,#'fl*)
        (,#'fl/       left      40  ,#'fl/)
        (,#'fl//      left      40  ,#'flquotient)
        (,#'fl%       left      40  ,#'flremainder)
        (,#'fl+       left      50  ,#'fl+)
        (,#'fl-       left      50  ,#'fl-)
        (,#'fl<       compare   80  ,#'fl<?)
        (,#'fl>       compare   80  ,#'fl>?)
        (,#'fl<=      compare   80  ,#'fl<=?)
        (,#'fl>=      compare   80  ,#'fl>=?)
        (,#'fl=       compare   80  ,#'fl=?)
        (,#'fl!=      left      90  ,#'expr-fl!=)
      ))

    ;-------------------------------------------------------
    ; procedural part
    ;-------------------------------------------------------

    ; trick to get distinct variables, to be changed
    (define (nvar)
      (let ((x (generate-temporaries '(x))))
        (car x)))

    ; default priority of calls
    (define call-priority 10)

    ; the less priority
    (define least-priority 1000)

    ; an operator definition, opdef, is a list of 4 elements:
    ; - identifier: identifier of the operator
    ; - type: one of the symbol left, right, list, comp, ternary, as, pair-of
    ; - priority: the priority of the operator
    ; - repl: the replacement of the identifier
    (define opdef-identifier car)
    (define opdef-type       cadr)
    (define opdef-priority   caddr)
    (define opdef-repl       cadddr)

    ; oper is a list of 2 elements:
    ; - item: the syntax object of the matched operator
    ; - def: the definition of the operator
    (define oper-item car)
    (define oper-def  cdr)
    (define make-oper cons)

    ; check if the 2 operator are the same
    (define (oper-eq? oper1 oper2)
      (and oper1
           oper2
           (eq? (oper-def oper1) (oper-def oper2))))

    ; get the type of oper
    (define (oper-type oper)
      (opdef-type (oper-def oper)))

    ; context is a list of 3 elements:
    ;  - item: the item that defines the context (opdef or priority)
    ;  - opdefs: definition of operations
    (define context-item   car)
    (define context-opdefs cadr)

    (define (make-context priority opdefs)
      (list priority opdefs))

    ; return the context with the given priority
    (define (context-of-priority context priority)
      (cons priority (cdr context)))

    ; get the context coming from applying oper
    (define (context-of-oper context oper)
      (cons (oper-def oper) (cdr context)))

    ; check if the oper applies in the context
    (define (oper-applies? oper context)
      (let* ((opdef (oper-def oper))
             (item  (context-item context))
             (prio  (if (number? item) item (opdef-priority item))))
        (or (> prio (opdef-priority opdef))
            (and (eq? item opdef) (eqv? (opdef-type item) 'right)))))

    ; test if oper2 is the second operator of a ternary operation
    ; started by oper1
    (define (pair-of? oper1 oper2)
      (let ((op1 (oper-def oper1))
            (op2 (oper-def oper2)))
        (and (eqv? (opdef-type op2) 'pair-of)
             (free-identifier=? (opdef-repl op2) (opdef-identifier op1)))))

    ; gets the ransformation of the operator 'oper' for the argument list 'args'
    (define (oper-apply oper args)
      (let ((tid  (oper-item oper))
            (repl (opdef-repl (oper-def oper))))
        (cond
          ((identifier? repl)
            (cons repl args))
          ((procedure? repl)
            (repl tid args))
          (else
            (error "bad operator definition" (oper-def oper))))))

    ; gets in 'context' the oper of given 'item'
    ; 'prefix?' is #true for a prefix operation, #false for others
    ; returns #f if there is no oper for 'item'
    (define (s-oper item context prefix?)
      (and (identifier? item)
           (let lp ((iter-opdefs (context-opdefs context)))
             (and (pair? iter-opdefs)
                  (let ((opdef (car iter-opdefs)))
                    (if (and (free-identifier=? item (opdef-identifier opdef))
                             (eqv? prefix? (eqv? 'prefix (opdef-type opdef))))
                      (make-oper item opdef)
                      (lp (cdr iter-opdefs))))))))

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
    (define (t-term item)
      (syntax-case item ()
        (((lst ...))  (syntax (lst ...)))
        ((lst ...)    (syntax (expr lst ...)))
        (_            item)))

    ; gets the transformed expression of the head of 'expression'
    ; in the given 'context'
    ; calls the continuation 'cont' with the replaced value of the
    ; first term and the remaining part: (cont repl rest)
    (define (t-prefix expression context cont)
      ; split in head and tail and search if head is a prefix operator
      (let* ((head (t-first expression))
             (rest (t-rest expression))
             (oper (s-prefix head context)))
        (if oper
          ; head is a prefix operator, get the term where it apply
          (t-prefix rest (context-of-oper context oper)
            (lambda (term rest)
              (t-cont (oper-apply oper (list term)) rest context cont)))
          ; head isn't a prefix operator so it is a term
          (t-cont (t-term head) rest context cont))))

    ; gets the transformation of 'term' followed by 'rest' in 'context'
    ; searches for an operator at begin of rest
    (define (t-cont term rest context cont)
      (t-args rest #t
        (lambda (args rest)
          (let ((term (if args (cons term args) term))
                (head (t-first rest)))
            (if (not head)
              (cont term rest)
              ; check if head of rest is an operator
              (let ((oper (s-infix head context)))
                (if (and oper (oper-applies? oper context))
                  ; operator found and priority
                  (t-infix term oper (t-rest rest) context cont)
                  (cont term rest))))))))

    ; gets the list of arguments
    (define (t-args expr null-ok? cont)
      (let ((head (t-first expr)))
        (syntax-case head ()
          (()
            (if null-ok?
              (cont '() (t-rest expr))
              (syntax-violation #f "empty argument forbiden" expr)))
          ((lst ...)
            (t-args (t-rest expr) #f
              (lambda (args rest)
                (cont (cons (t-term head) (or args '())) rest))))
          (_
            (cont #f expr)))))

    ; transform an infix operator 'oper' preceded by 'term' and followed
    ; by 'rest'
    (define (t-infix term oper rest context cont)
      ; detect empty rest
      (unless rest
        (syntax-violation #f "unexpected end after operator" (oper-item oper)))
      ; prepare the ccontext of the operation
      (let ((op-ctxt (context-of-oper context oper)))
        ; processing depends of the type of the operator
        (case (oper-type oper)

          ; for left associative operators
          ((left)
            (t-prefix rest op-ctxt
                    (lambda (second rest)
                      (let* ((args (list term second))
                             (item (oper-apply oper args)))
                        (t-cont item rest context cont)))))

          ; for right associative operators
          ((right)
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
          ((compare)
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
                            ; it is not the same, check if it is of type compare
                            (if (not (and nope (eqv? 'compare (oper-type nope))))
                              ; not of type compare, continue
                              (let* ((args (reverse (cons term rlst)))
                                     (item (oper-apply oper args)))
                                (t-cont item rest context cont))
                              ; it is of type compare
                              ; create a variable for holding current 'term' value
                              ; create the comparison of current operator with this variable
                              (let* ((var   (nvar))
                                     (args  (reverse (cons var rlst)))
                                     (comp1 (oper-apply oper args)))
                                ; scan the new comparison
                                (t-infix var nope (t-rest rest) context
                                  (lambda (comp2 rest)
                                    ; produce the result and continue
                                    (let* ((tid (oper-item oper))
                                           (s-let (datum->syntax tid 'let))
                                           (s-and (datum->syntax tid 'and))
                                           (item `(,s-let ((,var ,term)) (,s-and ,comp1 ,comp2))))
                                      (t-cont item rest context cont))))))))))))

          ; ternary as 3 parts, the third one is separated by the matching pair-of
          ((ternary)
            (letrec ((t-middle
                      (lambda (term2 rest)
                        ; scan head of rest until the paired operator
                        (let* ((head  (t-first rest))
                               (nope  (s-infix head context)))
                          ; test if head is the paired symbol
                          (if (not nope)
                            (syntax-violation #f "ternary not closed" (oper-item oper))
                            (if (not (pair-of? oper nope))
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
            ; get second expression, a list of items until paired symbol
            (let t-id ((term2 '())
                       (rest  rest))
              ; scan head
              (let ((head  (t-first rest))
                    (rest  (t-rest rest)))
                ; test if head is the paired symbol
                (if (not head)
                  (syntax-violation #f "as not closed" (oper-item oper))
                  (let ((nope  (s-infix head context)))
                    (if (and nope (pair-of? oper nope))
                      ; yes, get third expression and conclude
                      (t-prefix rest op-ctxt
                            (lambda (term3 rest)
                              (let* ((args (list term (reverse term2) term3))
                                     (item (oper-apply oper args)))
                                (t-cont item rest context cont))))
                      ; no, capture head in term2 and iterate
                      (t-id (cons head term2) rest)))))))

          ((pair-of)
            (syntax-violation #f "second of ternary without first" (oper-item oper)))

          (else
            (error "unexpected operator type" (oper-type oper) (oper-item oper) rest)))))

    ; root function for processing expression accordingly to operator definitiions
    (define (t-expr expression opdefs)
      (t-prefix expression (make-context least-priority opdefs)
        (lambda (result rest)
          (when rest
            (syntax-violation #f "remaining part" rest))
          result)))

    ; expr syntax root
    (lambda (x)
      (syntax-case x ()
        ((_ term ...)
         (t-expr #'(term ...) opdefs))))))


