(import
   (scheme base)
   (srfi 1)
)

; grammar is simply defined as below
(define GRAMMAR '(

   (EXPR
     (COND                       -> $1))
   (COND
     (OR "if" OR "else" OR       -> ("if" $3 $1 $5))
     (OR                         -> $1))
   (OR
     (AND (+ "or" AND)           -> ("or" $1 . $2))
     (AND                        -> $1))
   (AND
     (IMPLIES (+ "and" IMPLIES)  -> ("and" $1 . $2))
     (IMPLIES                    -> $1))
   (IMPLIES
     (EQV "implies" EQV          -> ("or" ("not" $1) $3))
     (EQV                        -> $1))
   (EQV
     (NOT "not" "eqv" NOT        -> ("not" ("eqv?" $1 $4)))
     (NOT "eqv" NOT              -> ("eqv?" $1 $3))
     (NOT                        -> $1))
   (NOT
     ("not" NOT                  -> ("not" $2))
     (COMP                       -> $1))
   (COMP
     (SUM (+ "<"  SUM)           -> ("<" $1 . $2))
     (SUM (+ "<=" SUM)           -> ("<=" $1 . $2))
     (SUM (+ ">"  SUM)           -> (">" $1 . $2))
     (SUM (+ ">=" SUM)           -> (">=" $1 . $2))
     (SUM "="  SUM               -> ("=" $1 $3))
     (SUM "<>" SUM               -> ("not" ("=" $1 $3)))
     (SUM                        -> $1))
   (SUM
     (DIFF (+ "+" DIFF)          -> ("+" $1 . $2))
     (DIFF                       -> $1))
   (DIFF
     (FACTOR (+ "-" FACTOR)      -> ("-" $1 . $2))
     (FACTOR                     -> $1))
   (FACTOR
     ("-" FACTOR                 -> ("-" $2))
     ("+" FACTOR                 -> $2)
     (MUL                        -> $1))
   (MUL
     (DIV (+ "*" DIV)            -> ("*" $1 . $2))
     (DIV                        -> $1))
   (DIV
     (QUOT (+ "/" QUOT)          -> ("/" $1 . $2))
     (QUOT                       -> $1))
   (QUOT
     (QUOT ":" REM               -> ("quotient" $1 $3))
     (REM                        -> $1))
   (REM
     (REM "%" EXP                -> ("remainder" $1 $3))
     (EXP                        -> $1))
   (EXP
     (TERM "^" TERM (+ "^" TERM) -> ("expt" $1 ("*" $3 . $4)))
     (TERM "^" TERM              -> ("expt" $1 $3))
     (TERM                       -> $1))
   (EXPT
     ("-" EXPT                   -> (- $2))
     ("+" EXPT                   -> $2)
     (TERM "^" EXPT              -> ("*" $1 $3))
     (TERM                       -> $1))
   (TERM
     (ITEM "@" ITEM              -> ("vector-ref" $1 $3))
     (ITEM "@." ITEM             -> ("list-ref" $1 $3))
     (ITEM "@@" ITEM             -> ("bytevector-u8-ref" $1 $3))
     (ITEM                       -> $1))
   (ITEM
     (((x ...))                  -> (x ...))
     ((x ...)                    -> ("expr" x ...))
     (x                          -> x))
))

#;(define GRAMMAR '(

   (EXPR
     (SUM "END"                    -> $1))
   (SUM
     (SUM "+" DIFF              -> ("+" $1  $3))
     (DIFF                       -> $1))
   (DIFF
     (DIFF "-" FACT              -> ("-" $1 $3))
     (FACT                       -> $1))
   (FACT
     (FACT "*" DIV              -> ("*" $1 $3))
     (DIV                       -> $1))
   (DIV
     (DIV "/" TERM              -> ("/" $1 $3))
     (DIV ":" TERM              -> ("quotient" $1 $3))
     (DIV "%" TERM              -> ("remainder" $1 $3))
     (TERM                       -> $1))
   (TERM
     ("-" TERM                   -> ("-" $2))
     ("+" TERM                   -> $2)
     (x                          -> x))
))

#;(define GRAMMAR '(

   (EXPR
     (SUM                    -> $1))
   (SUM
     (SUM "+" DIFF              -> ("+" $1  $3))
     (DIFF                       -> $1))
   (DIFF
     (DIFF "-" TERM              -> ("-" $1 $3))
     (TERM                       -> $1))
   (TERM
     ((x ...)                    -> ("expr" x))
     (x                          -> x))
))

;---------------------------------------
; specialized version of fold for one list
(define (fold1 kons knil list1)
   (if (null? list1)
      knil
      (fold1 kons (kons (car list1) knil) (cdr list1))))

; compare any values
(define (lesser? a b)
   (cond
      ((boolean? a)
         (or (not (boolean? b))
             (and (not a) b)))
      ((number? a)
         (or (not (number? b))
             (< a b)))
      ((string? a)
         (or (not (string? b))
             (string<? a b)))
      ((symbol? a)
         (or (not (symbol? b))
             (string<? (symbol->string a) (symbol->string b))))
      ((pair? a)
         (or (not (pair? b))
             (lesser? (car a) (car b))
             (and (equal? (car a) (car b))
                  (lesser? (cdr a) (cdr b)))))
      (else
         #f)))

; return a sorted list having item inserted
(define (sorted-list-add lst item)
   (if (or (null? lst) (lesser? item (car lst)))
      (cons item lst)
      (cons (car lst) (sorted-list-add (cdr lst) item))))

; transform x to its string
(define (to-string x)
   (cond
      ((string? x) x)
      ((symbol? x) (symbol->string x))
      ((number? x) (number->string x))
      (else "?")))

; return the symbol '$i' where i is the given number 'num'
(define (dollar num)
   (string->symbol (string-append "$" (number->string num))))

;=======================================
; Rules are made of
;  - a name
;  - a pattern (a list)
;  - a product
;  - a capture (a list)
;---------------------------------------
(define-record-type <RULE>
   (!rule! name pattern product capt)
   rule?
   (name     rule-name)          ; symbol
   (pattern  rule-pattern)       ; list of symbols
   (product  rule-product)       ; production of the rule
   (capt     rule-capt)          ; capture list
)

; create a rule
(define (make-rule name pattern product)
   (let ((capt (let loop ((iter pattern)
                          (idx  1)
                          (capt '()))
                        (if (null? iter)
                           capt
                           (loop (cdr iter) (+ idx 1)
                              (if (string? (car iter))
                                 capt
                                 (cons (dollar idx) capt)))))))
;(for-each display (list "xxx " name ": " pattern " -> " product " = " capt "\n"))
      (!rule! name pattern product capt)))

;=======================================
; Normalisation of rules
; all the story of normalisation of rules is to
; transform the grammar given as a big s-expr
; almost readable if correctly indented to
; the internal data structure used by algorithms
;---------------------------------------
; generation of temporary unique names
(define tmp-name-count 0)
(define (make-tmp-name name)
   (let* ((num    (+ tmp-name-count 1))
          (numstr (number->string num))
          (namstr (symbol->string name))
          (tmpstr (string-append namstr "~" numstr))
          (tmpnam (string->symbol tmpstr)))
      (set! tmp-name-count num)
      tmpnam))

; return (values pattern product)
; from a rule (pattern -> product)
; ex: (a b -> x y) -> (a b) (x y)
(define (split-rule rule)
   (let ((head (car rule))
         (tail (cdr rule)))
      (if (eq? '-> head)
         (values '() (car tail))
         (let-values (((pattern product) (split-rule tail)))
            (values (cons head pattern) product)))))

; given a plus pattern of type (A B) and a name
; returns the ruleset
;  (name (A B name) -> ($2 . $3))
;  (name (A B)      -> ($2))
(define (make-plus-rules name pattern)
   (let ((a (car pattern))
         (b (cadr pattern)))
      (list
         (make-rule name (list a b name) '($2 . $3))
         (make-rule name (list a b)      '($2)))))

; if the pattern has no (+ A B) rule, returns #f, #f
; other wise, replace (+ A B) by a reference to a new
; rule ans returns NP, NRS where NP is the new pattern
; replacing the previous on and NRS is the rule set (a list)
; for the new rule
(define (expand-pattern-plus name pattern)
   (let lp ((iter pattern))
      (if (null? iter)
         ; not found
         (values #f #f)
         (let ((head (car iter))
               (tail (cdr iter)))
            ; check if head match (+ ...)
            (if (and (pair? head) (eq? '+ (car head)))
               ; yes, return the replacement
               (let* ((tname   (make-tmp-name name))
                      (ruleset (make-plus-rules tname (cdr head))))
                  (values (cons tname (cdr iter)) ruleset))
               ; no, look further and report result
               (let-values (((npat ruleset) (lp tail)))
                  (if npat
                     (values (cons head npat) ruleset)
                     (values #f #f))))))))

; look to the rule for (+ ...) in pattern
; return #f if not found
; if found, returns a rule set replacing the rule
(define (expand-rule-plus rule)
   (let ((name    (rule-name rule))
         (pattern (rule-pattern rule))
         (product (rule-product rule)))
      (let-values (((newpat ruleset) (expand-pattern-plus name pattern)))
         (and newpat
              (cons (make-rule name newpat product) ruleset)))))

; expands plus rules (+ ...) to create
; a new ruleset not containing it
(define (expand-ruleset-plus ruleset)
   (if (null? ruleset)
      ruleset
      (let* ((tail  (cdr ruleset))
             (ntail (expand-ruleset-plus tail))
             (rule  (car ruleset))
             (nrule (expand-rule-plus rule)))
         (if nrule
            (append (expand-ruleset-plus nrule) ntail)
            (cons rule ntail)))))

; transform the original ruleset to the internal ruleset
; ex: (NAME (PAT -> PRO) ...) -> (NAME (PAT)  PRO) ...
(define (transform-ruleset ruleset)
   (if (null? ruleset)
      ruleset
      (let ((seed   (transform-ruleset (cdr ruleset)))
            (name   (caar ruleset))
            (subs   (cdar ruleset)))
         (fold1
            (lambda (rule seed)
               (let-values (((pattern product) (split-rule rule)))
                  (cons (make-rule name pattern product) seed)))
            seed
            (reverse subs)))))

; transform the original ruleset to the internal ruleset
(define (normalize-ruleset ruleset)
   (expand-ruleset-plus (transform-ruleset ruleset)))

;=======================================
; record of the grammar
;---------------------------------------
; using record for grammars
(define-record-type <GRAMMAR>
   (!grammar! root non-terminals terminals ruleset firstls)
   grammar?
   (root          grammar-root)           ; symbol
   (non-terminals grammar-non-terminals)  ; list of not terminal symbols
   (terminals     grammar-terminals)      ; list of terminal symbols
   (ruleset       grammar-ruleset)        ; vector of rules (name (pattern ...) prodction ...)
   (firstls       grammar-firstls grammar-set-firstls!)) ;


; make a grammar from a set of rules
(define (make-grammar rules)
   (let* ((root      (caar rules))
          ; normalize the ruleset
          (pre-rs    (normalize-ruleset rules))
          (ruleset   (list->vector pre-rs))
          ; extract non terminals
          (nterms    (let lp ((ls (map rule-name pre-rs)))
                        (if (null? ls)
                           '()
                           (let ((head (car ls))(tail (cdr ls)))
                              (if (member head tail)
                                 (lp tail)
                                 (cons head (lp tail)))))))
          ; extract terminals
          (terms     (let lp ((ls (apply append (map rule-pattern pre-rs))))
                        (if (null? ls)
                           '()
                           (let ((head (car ls))(tail (cdr ls)))
                              (if (or (member head nterms) (member head tail))
                                 (lp tail)
                                 (cons head (lp tail))))))))
      ; make the grammar now
      (!grammar! root nterms terms ruleset '())))

; test if symbol is non terminal
(define (grammar-non-terminal? grammar symbol)
   (member symbol (grammar-non-terminals grammar)))

; test if symbol is terminal
(define (grammar-terminal? grammar symbol)
   (member symbol (grammar-terminals grammar)))

; the count of rules
(define (grammar-rule-count grammar)
   (vector-length (grammar-ruleset grammar)))

; the ith rule
(define (grammar-rule grammar ith)
   (vector-ref (grammar-ruleset grammar) ith))

; the ith name
(define (grammar-name grammar ith)
   (rule-name (grammar-rule grammar ith)))

; the ith pattern
(define (grammar-pattern grammar ith)
   (rule-pattern (grammar-rule grammar ith)))

; the ith product
(define (grammar-product grammar ith)
   (rule-product (grammar-rule grammar ith)))

; get the symbol of ith rule at pos
(define (grammar-at grammar ith pos)
   (let ((pattern (grammar-pattern grammar ith)))
      (and (< pos (length pattern))
           (list-ref pattern pos))))

; call (proc rule seed) for all rules of name symbol
(define (grammar-fold-named-rule grammar symbol seed proc)
   (let loop ((idx  (- (grammar-rule-count grammar) 1))
              (seed seed))
      (if (negative? idx)
         seed
         (let ((rule (grammar-rule grammar idx)))
            (loop
               (- idx 1)
               (if (equal? symbol (rule-name rule))
                  (proc rule seed)
                  seed))))))

; return list of first terminals for symbol
(define (%grammar-firsts grammar symbol)
   (let add ((symbol symbol)
             (seed   '())
             (trail  '()))
      (if (grammar-terminal? grammar symbol)
         (if (member symbol seed)
            seed
            (sorted-list-add seed symbol))
         (grammar-fold-named-rule grammar symbol seed
            (lambda (rule seed)
               (let ((sym (car (rule-pattern rule))))
                  (if (member sym trail)
                     seed
                     (add sym seed (cons sym trail)))))))))

(define (grammar-firsts grammar symbol)
   (cond
      ((assoc symbol (grammar-firstls grammar))
         => cdr)
      (else
         (let ((result (%grammar-firsts grammar symbol)))
            (grammar-set-firstls! grammar
               (cons (cons symbol result) (grammar-firstls grammar)))
            result))))

(define (display-grammar grammar)
   (display "GRAMMAR ")
   (write (grammar-root grammar))
   (newline)
   (display "  non-term ")
   (write (grammar-non-terminals grammar))
   (newline)
   (display "  term ")
   (write (grammar-terminals grammar))
   (newline)
   (display "  rules")
   (newline)
   (vector-for-each
      (lambda (rule)
         (display "      ")
         (write rule)
         (newline))
      (grammar-ruleset grammar))
   (newline))

;=======================================
; A scanning position, scapo, is a list
; whose first element is the position
; of scanning within a rule and whose next
; elements are the symbols in advance if any
;---------------------------------------
; make a scapo
(define (make-scapo pos symbols)
   (cons pos symbols))

; get scanning position of scapo
(define (scapo-pos scapo)
   (car scapo))

; get symbols of scapo
(define (scapo-symbols scapo)
   (cdr scapo))

; returns the scapo for the next position
(define (scapo-next scapo)
   (make-scapo (+ 1 (scapo-pos scapo)) (scapo-symbols scapo)))

; compare 2 scapo
(define (scapo<? scapo-a scapo-b)
   (lesser? scapo-a scapo-b))

;=======================================
; A rule state, rusta, is a pair (#rule . scapo)
; or in other words, a list (#rule pos symb ...)
; where #rule is the index of the rule,
; pos is the scanning position
; symb ... a sorted list of symbols
;---------------------------------------
; make a rusta
(define (make-rusta irule scapo)
   (cons irule scapo))

; get rule index of rusta
(define (rusta-irule rusta)
   (car rusta))

; get scapo of rusta
(define (rusta-scapo rusta)
   (cdr rusta))

; get pos of scapo of rusta
(define (rusta-pos rusta)
   (cadr rusta))

; get symbols of scapo of rusta
(define (rusta-symbols rusta)
   (cddr rusta))

; compare 2 rusta
(define (rusta<? rusta-a rusta-b)
   (lesser? rusta-a rusta-b))

;=======================================
; a rustaset is an ordered list of rusta.
;---------------------------------------
; equality of rustaset
(define (rustaset=? a b)
   (equal? a b))

; return all symbols accepted by the rustaset
(define (state-starting-symbols rustaset grammar)
   (if (null? rustaset)
      '()
      (let* ((rusta (car rustaset))
             (symb  (grammar-at grammar (car rusta) (cadr rusta)))
             (tail  (state-starting-symbols (cdr rustaset) grammar)))
         (if (or (not symb) (member symb tail))
            tail
            (cons symb tail)))))

;=======================================
; stabu is the rustaset builder, it is an array
; each item of the vector matches the rule
; of the grammar of sme index
; for each rule, the list contains the
; scanning positions, scapo, of the rule, sorted
; in increasing order.
; a scanning position, scapo, is a list of the position
; and the symbols in advances.
; ex: if the rule is (A B C), the scanning position
; (1 "+") means (A . B C) with "+" in advance.
;---------------------------------------
; create a stabu
(define (make-stabu grammar)
   (make-vector (grammar-rule-count grammar) '()))

; init stabu from the rustaset
(define (stabu-set! stabu rustaset)
   ; reset stabu
   (vector-fill! stabu '())
   (let lp ((iter rustaset))
      (unless (null? iter)
         (let ((rusta (car iter)))
            (stabu-add! stabu (car rusta) (cdr rusta))
            (lp (cdr iter))))))

; add a position to stabu, return #t if added, #f if already in stabu
(define (stabu-add! stabu irule scapo)
   (let ((prv (vector-ref stabu irule)))
      (if (member scapo prv)
         #f
         (let ((val (let lp ((iter prv))
                        (if (or (null? iter) (scapo<? scapo (car iter)))
                           (cons scapo iter)
                           (cons (car iter) (lp (cdr iter)))))))
            (vector-set! stabu irule val)
            #t))))

; call proc with decreasing irule scapo
; the procedure is called with 3 arguments
; (proc idx scapo seed) and should return the
; new seed
(define (stabu-fold-down stabu seed proc)
   (let lp ((idx  (- (vector-length stabu) 1))
            (seed seed))
      (if (negative? idx)
         seed
         (lp (- idx 1)
             (let lp2 ((lspos (vector-ref stabu idx)))
               (if (null? lspos)
                  seed
                  (proc idx (car lspos) (lp2 (cdr lspos)))))))))

; call proc with increasing irule scapo
; the procedure is called with 3 arguments
; (proc idx scapo seed) and should return the
; new seed
(define (stabu-fold-up stabu seed proc)
   (let lp ((idx  0)
            (seed seed))
      (if (>= idx (vector-length stabu))
         seed
         (lp (+ idx 1)
             (let lp2 ((lspos (vector-ref stabu idx))
                       (seed  seed))
               (if (null? lspos)
                  seed
                  (lp2 (cdr lspos) (proc idx (car lspos) seed))))))))

; create the rustaset from stabu
(define (stabu->rustaset stabu)
   (stabu-fold-down
      stabu
      '()
      (lambda (idx scapo rustaset)
         (cons (cons idx scapo) rustaset))))

; add entering the rules of symbol
(define (stabu-enter! stabu grammar symbol nxtsym)
   (let lp ((idx  (- (vector-length stabu) 1))
            (chg  #f))
      (if (negative? idx)
         chg
         (lp (- idx 1)
             (if (equal? symbol (grammar-name grammar idx))
               (let* ((scapo (make-scapo 0 nxtsym))
                      (added (stabu-add! stabu idx scapo)))
                  (or added chg))
               chg)))))

; fullfil the stabu in order to stick on non-terminals
(define (stabu-close! stabu grammar)
   (define (lsnsymbs pnsym nxsym)
      (if (null? pnsym)
         '(())
         (if nxsym
            (map list (grammar-firsts grammar nxsym))
            (list pnsym))))
   (let ((chg (stabu-fold-up
                  stabu
                  #f
                  (lambda (idx scapo chg)
                     (let* ((pos  (scapo-pos scapo))
                            (symb (grammar-at grammar idx pos)))
                        (if symb
                           (let* ((pnsym  (scapo-symbols scapo))
                                  (nxsym  (grammar-at grammar idx (+ pos 1)))
                                  (lst    (lsnsymbs pnsym nxsym)))
                              (fold1
                                 (lambda (lssym chg)
                                    (or (stabu-enter! stabu grammar symb lssym)) chg)
                                 chg
                                 lst))
                           chg))))))
      (when chg
         (stabu-close! stabu grammar))))

; compute stabu when symbol is shift out
(define (stabu-shift! stabu grammar symbol)
   ; function shifting a list of scapo
   (letrec ((filt (lambda (grammar idx lsscapo)
                     (if (null? lsscapo)
                        '()
                        (let ((scapo (car lsscapo))
                              (tail  (filt grammar idx (cdr lsscapo))))
                           (if (equal? symbol (grammar-at grammar idx (scapo-pos scapo)))
                              (cons (scapo-next scapo) tail)
                              tail))))))
      ; for all states
      (let lp ((idx  (- (vector-length stabu) 1)))
         (unless (negative? idx)
            (vector-set! stabu idx (filt grammar idx (vector-ref stabu idx)))
            (lp (- idx 1))))))

;=======================================
; a shift is a symbol and a target
;---------------------------------------
(define-record-type <SHIFT>
   (!shift! symbol target)
   shift?
   (symbol   shift-symbol)
   (target   shift-target shift-set-target!))

(define (make-shift symbol target)
   (!shift! symbol target))

;=======================================
; a sred is a symbol and a reducing rule
;---------------------------------------
(define-record-type <SRED>
   (!sred! symbol rule)
   sred?
   (symbol  sred-symbol)
   (rule    sred-rule))

(define (make-sred symbol rule)
   (!sred! symbol rule))

;=======================================
; bausta is a basic automata state, its made of:
; - a number
; - a rustaset
; - possible shifts (list of symbol x bausta)
; - possible reduces (list of irul)
;---------------------------------------
; the bausta record
(define-record-type <BAUSTA>
   (!bausta! number rustaset shifts reduces shifreds)
   bausta?
   (number   bausta-number bausta-set-number!)
   (rustaset bausta-rustaset)
   (shifts   bausta-shifts bausta-set-shifts!)
   (reduces  bausta-reduces bausta-set-reduces!)
   (shifreds bausta-shifreds bausta-set-shifreds!))

; creates a bausta for the rustaset
(define (make-bausta grammar number rustaset)
   (let lp ((iter rustaset)
            (reds '())) ;; computes the reduces
      (if (null? iter)
         (!bausta! number rustaset '() reds '())
         (lp
            (cdr iter)
            (let* ((rusta (car iter))
                   (irule (rusta-irule rusta))
                   (symb  (grammar-at grammar irule (rusta-pos rusta))))
               (if (or symb (member (caar iter) reds))
                  reds
                  (cons (grammar-rule grammar irule) reds)))))))

; get starting symbols of bausta
(define (bausta-symbols bausta grammar)
   (state-starting-symbols (bausta-rustaset bausta) grammar))

; add shift to an other bausta on symbol
(define (bausta-add-shift! bausta symbol other-bausta)
   (let ((sft  (make-shift symbol other-bausta))
         (lsft (bausta-shifts bausta)))
      (bausta-set-shifts! bausta (cons sft lsft))))

; reverse shifts and reduces
(define (bausta-rsr bausta)
   (bausta-set-shifts! bausta (reverse (bausta-shifts bausta)))
   (bausta-set-reduces! bausta (reverse (bausta-reduces bausta))))

; checks if bausta matches rustaset
(define (bausta-matches? bausta rustaset)
   (equal? (bausta-rustaset bausta) rustaset))

; get start symbol of a rule
(define (bausta-rule-symbol bausta grammar rule)
   (fold1
      (lambda (rusta symbol)
         (or symbol
            (let* ((irule (rusta-irule rusta))
                   (grule (grammar-rule grammar irule)))
               (and (eq? grule rule)
                    (grammar-at grammar irule (rusta-pos rusta))))))
      #f
      (bausta-rustaset bausta)))

; get the first shift of symbol
(define (bausta-find-shift bausta symbol)
   (let lp ((iter (bausta-shifts bausta)))
      (and (pair? iter)
           (if (equal? symbol (shift-symbol (car iter)))
               (car iter)
               (lp (cdr iter))))))

; get the first shifred of symbol
(define (bausta-find-shifred bausta symbol)
   (let lp ((iter (bausta-shifreds bausta)))
      (and (pair? iter)
           (if (equal? symbol (sred-symbol (car iter)))
               (car iter)
               (lp (cdr iter))))))

; displays a bausta
(define (bausta-display bausta grammar)
   ; display the name
   (display "STATE ")
   (display (bausta-number bausta))
   (newline)
   (display "- PATTERNS")
   (newline)
   ; display each active pattern
   (let loop ((rustaset (bausta-rustaset bausta)))
      (when (pair? rustaset)
         (let* ((rusta   (car rustaset))
                (idx     (rusta-irule rusta))
                (name    (grammar-name grammar idx))
                (pattern (grammar-pattern grammar idx))
                (len     (length pattern)))
            ; display one irule pattern
            (display "   ")
            (display name)
            (display ":")
            (let lp ((cp      0)
                     (pos     (rusta-pos rusta))
                     (pattern pattern)
                     (rustaset   (cdr rustaset)))
               (if (< cp pos)
                  (begin
                     ; not at scanning position
                     (display " ")
                     (write (car pattern))
                     (lp (+ cp 1) pos (cdr pattern) rustaset))
                  (begin
                     ; at scanning position
                     (display " .")
                     ; display the remaining of the pttern and loop
                     (let lp2 ((pattern pattern))
                        (if (null? pattern)
                           (begin
                              (when (pair? (rusta-symbols rusta))
                                 (display "   [")
                                 (write (rusta-symbols rusta))
                                 (display "]"))
                              (newline)
                              (loop rustaset))
                           (begin
                              (display " ")
                              (write (car pattern))
                              (lp2 (cdr pattern)))))))))))
   ; display the shifts
   (unless (null? (bausta-shifts bausta))
      (display "- SHIFTS")
      (newline)
      (let loop ((lssft (bausta-shifts bausta)))
         (unless (null? lssft)
            (display "   ")
            (write (shift-symbol (car lssft)))
            (display " shifts to ")
            (display (bausta-number (shift-target (car lssft))))
            (newline)
            (loop (cdr lssft)))))
   ; display the shift-reduces
   (unless (null? (bausta-shifreds bausta))
      (display "- SHIFT-REDUCES")
      (newline)
      (let loop ((lsred (bausta-shifreds bausta)))
         (unless (null? lsred)
            (display "   ")
            (write (sred-symbol (car lsred)))
            (display " shift-reduces/")
            (display (length (rule-pattern (sred-rule (car lsred)))))
            (display " as ")
            (display (rule-name (sred-rule (car lsred))))
            (display " producing ")
            (write (rule-product (sred-rule (car lsred))))
            (newline)
            (loop (cdr lsred)))))
   ; display the reduces
   (unless (null? (bausta-reduces bausta))
      (display "- REDUCES")
      (newline)
      (let loop ((lsred (bausta-reduces bausta)))
         (unless (null? lsred)
            (display "   ")
            (display "reduces/")
            (display (length (rule-pattern (car lsred))))
            (display " as ")
            (display (rule-name (car lsred)))
            (display " producing ")
            (write (rule-product (car lsred)))
            (newline)
            (loop (cdr lsred)))))
   (newline))

;=======================================
; a automaton is a grammar and a vector of bausta
;---------------------------------------
(define-record-type <AUTO>
   (!auto! grammar states)
   auto?
   (grammar auto-grammar)
   (states  auto-states auto-set-states!))

(define (make-auto grammar states)
   (!auto! grammar (list->vector states)))

(define (display-auto auto)
   (vector-for-each
      (lambda (bausta)
         (bausta-display bausta (auto-grammar auto))
         (newline))
      (auto-states auto)))

;=======================================
; bauto is a basic automaton made of a grammar
; and a list of i-sta bausta
;---------------------------------------
; build an automaton
(define (build-auto grammar make-bausta0 add-baustas)
   ; create stabu for working memory and compute initial state
   (let* ((stabu   (make-stabu grammar))
          (bausta0 (make-bausta0 grammar stabu)))
      (let loop ((bauto   (list bausta0))
                 (scan    (list bausta0)))
;(for-each display (list "s " (length bauto) " - " (length scan) "\n"))
         (if (null? scan)
            (begin
               (for-each bausta-rsr bauto)
               (make-auto grammar (reverse bauto)))
            (let* ((bausta    (car scan))
                   (nxt-bauto (fold1
                                 (lambda (symbol bauto)
                                    (add-baustas grammar stabu bauto bausta symbol))
                                 bauto
                                 (bausta-symbols bausta grammar)))
                   (new-scan  (rev-front-diff nxt-bauto bauto))
                   (nxt-scan  (append-in-place (cdr scan) new-scan)))
               (loop nxt-bauto nxt-scan))))))

; return a new list made of items at front of new-lst not in old-lst
; the items are returned in reverse order
(define (rev-front-diff new-lst old-lst)
   (let loop ((result  '())
              (new-lst new-lst))
      (if (eq? new-lst old-lst)
         result
         (loop (cons (car new-lst) result) (cdr new-lst)))))

; return the result of appending end to dest
; the end is appended in place when dest isn't null
(define (append-in-place dest end)
   (if (null? dest)
      end
      (begin
         (unless (null? dest)
            (let loop ((iter dest))
               (if (null? (cdr iter))
                  (set-cdr! iter end)
                  (loop (cdr iter)))))
         dest)))

;=======================================
; bauto is a basic automaton
; it is a list of bausta
;---------------------------------------
; search the bausta for the given rustaset
(define (bauto-search-bausta bauto rustaset)
   (let loop ((bauto bauto))
      (and (pair? bauto)
           (let ((bausta (car bauto)))
               (if (bausta-matches? bausta rustaset)
                  bausta
                  (loop (cdr bauto)))))))

; create a bausta that can be added to bauto
(define (bauto-make-bausta grammar bauto rustaset)
   (make-bausta grammar (length bauto) rustaset))

; add a shift of symbol from bausta to the bausta of the rustaset
; return the updated bauto
(define (bauto-add-shift grammar bauto bausta symbol rustaset)
   (let ((target-bausta (bauto-search-bausta bauto rustaset)))
      (if target-bausta
         (begin
            ; yes, found
            (bausta-add-shift! bausta symbol target-bausta)
            bauto)
         (let ((target-bausta (bauto-make-bausta grammar bauto rustaset)))
            (bausta-add-shift! bausta symbol target-bausta)
            (cons target-bausta bauto)))))

;=======================================
; bauto is a basic automaton made of a grammar
; and a list of i-sta bausta
;---------------------------------------
; build a LR0 automaton
(define (build-bauto-lr0 grammar)
   (build-auto grammar make-bausta0-lr0 build-next-bausta-for-symbol-lr0))

; create the initial state for LR0 automaton
(define (make-bausta0-lr0 grammar stabu)
   (vector-fill! stabu '())
   (stabu-enter! stabu grammar (grammar-root grammar) '())
   (stabu-close! stabu grammar)
   (bauto-make-bausta grammar '() (stabu->rustaset stabu)))

; build new states
(define (build-next-bausta-for-symbol-lr0 grammar stabu bauto bausta symbol)
   ; get the state after consuming symbol
   (stabu-set!   stabu (bausta-rustaset bausta))
   (stabu-shift! stabu grammar symbol)
   (stabu-close! stabu grammar)
   ; search if existing
   (let ((rustaset (stabu->rustaset stabu)))
      (bauto-add-shift grammar bauto bausta symbol rustaset)))

;=======================================
; bauto is a basic automaton made of a grammar
; and a list of i-sta bausta
;---------------------------------------
; build a LR0 automaton
(define (build-bauto-lr1 grammar)
   (build-auto grammar make-bausta0-lr1 build-next-bausta-for-symbol-lr1))

; create the initial state for LR0 automaton
(define (make-bausta0-lr1 grammar stabu)
   (vector-fill! stabu '())
   (stabu-enter! stabu grammar (grammar-root grammar) '(END))
   (stabu-close! stabu grammar)
   (bauto-make-bausta grammar '() (stabu->rustaset stabu)))

; build new states
(define (build-next-bausta-for-symbol-lr1 grammar stabu bauto bausta symbol)
   ; get the state after consuming symbol
   (stabu-set!   stabu (bausta-rustaset bausta))
   (stabu-shift! stabu grammar symbol)
   (stabu-close! stabu grammar)
   ; search if existing
   (let ((rustaset (stabu->rustaset stabu)))
      (bauto-add-shift grammar bauto bausta symbol rustaset)))

;=======================================
;=======================================
;=======================================
; extract from shifts of auto the vector
; whose elements are list of predecessors number
(define (compute-predecessors auto)
   (let* ((states (auto-states auto))
          (count  (vector-length states))
          (result (make-vector count '())))
      (do ((i 0 (+ i 1)))
         ((= i count) result)
         (for-each
            (lambda (sft)
               (let* ((ito (bausta-number (shift-target sft)))
                      (lst (vector-ref result ito))
                      (lst (cons i lst)))
                  (vector-set! result ito lst)))
            (bausta-shifts (vector-ref states i))))))

; find the list of predecessors of state
; idx at the count level
(define (pred-set idx predecessors count)
   (let lp ((count  count)
            (idxs   (list idx)))
      (if (zero? count)
         idxs
         (lp
            (- count 1)
            (fold1
               (lambda (i s)
                  (lset-union = s (vector-ref predecessors i)))
               '()
               idxs)))))

;=======================================
;=======================================
;=======================================
(define (simplify-auto auto)
   (remove-simplifiable-states auto))

; check if state bausta is simplifiable
; simplifiable states are states without shifts
; with only one reduce rule
; returns either false or the reduced rule
(define (bausta-simplifiable? bausta grammar)
   (let* ((sfts (bausta-shifts bausta))
          (reds (bausta-reduces bausta)))
      (and (null? sfts)
           (= 1 (length reds))
           (car reds))))

; removes the simplifiable states and adapt the shifts
; to reflect it
(define (remove-simplifiable-states auto)
   (let ((preds   (compute-predecessors auto))
         (states  (auto-states auto))
         (grammar (auto-grammar auto)))

      ; replace the shift to simplifiable states
      (let loop ((idx (vector-length states)))
         (unless (zero? idx)
            (let* ((idx  (- idx 1))
                   (bsta (vector-ref states idx))
                   (rule (bausta-simplifiable? bsta grammar)))
               (when rule
                  (for-each
                     (lambda (iprd)
                        (let* ((psta (vector-ref states iprd))
                               (sfts (bausta-shifts psta))
                               (symb (rule-name rule))
                               (srds (let lp ((sfts sfts))
                                       (if (null? sfts)
                                          (bausta-shifreds psta)
                                          (let ((sf   (car sfts))
                                                (srds (lp (cdr sfts))))
                                             (if (eq? bsta (shift-target sf))
                                                (cons
                                                   (make-sred
                                                      (shift-symbol sf)
                                                      rule)
                                                   srds)
                                                srds))))))
                           (bausta-set-shifreds! psta srds)))
                     (vector-ref preds idx))
                  (vector-set! preds idx #f))
               (loop idx))))

      ; remove the shift to simplified states
      (let loop ((idx (vector-length states)))
         (unless (zero? idx)
            (let* ((idx  (- idx 1))
                   (bsta (vector-ref states idx))
                   (sfts (fold-right
                           (lambda (sf rem)
                              (if (vector-ref preds (bausta-number (shift-target sf)))
                                 (cons sf rem)
                                 rem))
                           '()
                           (bausta-shifts bsta))))
               (bausta-set-shifts! bsta sfts)
               (loop idx))))

      ; compact the automaton
      (compact-auto auto preds)))

;
(define (compact-auto auto use)
   (let* ((states (auto-states auto))
          (count  (vector-length states)))
      (let loop ((idx  0)
                 (cnt  0))
         (if (< idx count)
            (if (vector-ref use idx)
               (begin
                  (vector-set! use idx cnt)
                  (loop (+ idx 1) (+ cnt 1)))
               (loop (+ idx 1) cnt))
            (let ((sta  (make-vector cnt)))
               (let loop ((idx  0))
                  (when (< idx count)
                     (let ((num (vector-ref use idx))
                           (bsta (vector-ref states idx)))
                        (when num
                           (bausta-set-number! bsta num)
                           (vector-set! sta num bsta))
                        (loop (+ idx 1)))))
               (auto-set-states! auto sta)
               auto)))))




;=======================================
;=======================================
;=======================================
; state-X   expr stack-state stack-terms
; red-X     stack-state stack-terms expr

(define add-comment #f)

(define (show-automaton auto)
   (let* ((predecessors  (compute-predecessors auto)))
      (vector-for-each
         (lambda (bausta)
            (show-bausta bausta auto predecessors))
         (auto-states auto))))

(define (showed-state-number number)
   (string-append "state-" (to-string number)))

(define (show-bausta bausta auto predecessors)
   (show-bausta-terminals bausta auto predecessors)
   (show-bausta-non-terminals bausta auto predecessors))

(define (show-bausta-terminals bausta auto predecessors)
   (display "(define-syntax state-")
   (display (bausta-number bausta))
   (newline)
   (when (> (length (bausta-reduces bausta)) 1)
      (display "   ; !!! MORE THAN ONE REDUCTION !!!")
      (newline))
   ; syntax rules with symbols
   (display "   (syntax-rules (")
   (fold1
      (lambda (sred first)
         (if (string? (sred-symbol sred))
            (begin
               (unless first
                  (display " "))
               (display (sred-symbol sred))
               #f)
            first))
      (fold1
         (lambda (shift first)
            (if (string? (shift-symbol shift))
               (begin
                  (unless first
                     (display " "))
                  (display (shift-symbol shift))
                  #f)
               first))
         #t
         (bausta-shifts bausta))
      (bausta-shifreds bausta))
   (display ")")
   (newline)
   ; shift rules
   (for-each
      (lambda (shift) (show-shift-if-terminal bausta auto shift))
      (bausta-shifts bausta))
   ; shift-reduce rules
   (for-each
      (lambda (shifred) (show-shifred-if-terminal bausta auto shifred predecessors))
      (bausta-shifreds bausta))
   ; reduce rules
   (if (null? (bausta-reduces bausta))
      (begin
         (display "      ((_ x ...)\t(syntax-error \"state-")
         (display (bausta-number bausta))
         (display ": invalid expression\" x ...))")
         (newline))
      (for-each
         (lambda (redu) (show-reduce bausta auto redu predecessors))
         (bausta-reduces bausta)))
   (display "))")
   (newline)
   (newline))

(define (show-bausta-non-terminals bausta auto predecessors)
   ; the unaliasing shift reduces
   (for-each
      (lambda (sred)
         (let ((symb    (sred-symbol sred))
               (rule    (sred-rule sred)))
            (when (and (grammar-non-terminal? (auto-grammar auto) symb)
                       (not (sred-alias? sred)))
               (when add-comment
                  (display "; ")
                  (display (bausta-number bausta))
                  (display " SHIFT ")
                  (display symb)
                  (display " REDUCE ")
                  (display (rule-product rule))
                  (display " AS ")
                  (display (rule-name rule))
                  (newline))
               (display "(define-syntax state-")
               (display (bausta-number bausta))
               (display "-")
               (display symb)
               (newline)
               ; syntax rules
               (display "   (syntax-rules ()")
               (newline)
               (show-red-sred bausta auto rule predecessors #t #f)
               (display "))")
               (newline)
               (newline))))
      (bausta-shifreds bausta)))

(define (show-shift-if-terminal bausta auto shift)
   (let ((grammar (auto-grammar auto))
         (symb    (shift-symbol shift))
         (targ    (shift-target shift)))
      (unless (grammar-non-terminal? grammar symb)
         (when add-comment
            (display "      ; SHIFT ")
            (display symb)
            (display " -> state-")
            (display (bausta-number targ))
            (newline))
         (display "      ((_ (")
         (display symb)
         (display " E ...) S T)")
         (display "\t")
         (display "(state-")
         (display (bausta-number targ))
         (display " (E ...) ")
         (display "(")
         (display (bausta-number bausta))
         (display " . S) ")
         (if (string? symb)
            (display "T")
            (begin
               (display "(")
               (if (null? (bausta-shifts targ))
                  (let* ((rusta  (car (bausta-rustaset targ)))
                         (irule  (rusta-irule rusta))
                         (prod   (grammar-product grammar irule)))
                     (display prod))
                  (display symb))
               (display " . T)")))
         (display "))")
         (newline))))

(define (show-shifred-if-terminal bausta auto sred preds)
   (let ((grammar (auto-grammar auto))
         (sftsymb (sred-symbol sred))
         (rule    (sred-rule sred)))
      (unless (grammar-non-terminal? grammar sftsymb)
         (when add-comment
            (display "      ; SHIFT ")
            (display sftsymb)
            (display " REDUCE ")
            (display (rule-product rule))
            (display " AS ")
            (display (rule-name rule))
            (newline))
         (show-red-sred bausta auto rule preds #t #t))))

(define (show-reduce bausta auto rule preds)
   (when add-comment
      (display "      ; REDUCE ")
      (display (rule-product rule))
      (display " AS ")
      (display (rule-name rule))
      (newline))
   (show-red-sred bausta auto rule preds #f #f))



(define (for-compacted-reduces auto symbol toistaset proc)
   (let ((rawlist
      (fold1
         (lambda (ista resu)
            (let-values (((usymbol toista) (resolve-sred auto ista symbol)))
               (let lp ((iter resu))
                  (if (null? iter)
                     (list (list usymbol toista ista))
                     (let* ((head1 (car iter))
                            (head2 (cdr head1)))
                        (if (and (equal? usymbol (car head1))
                                 (equal? toista  (car head2)))
                           (let sl ((head2 head2))
                              (let ((nxt (cdr head2)))
                                 (if (or (null? nxt) (< ista (car nxt)))
                                    (set-cdr! head2 (cons ista nxt))
                                    (sl nxt))))
                           (set-cdr! iter (lp (cdr iter))))
                        iter)))))
         '()
         toistaset)))
      (let ((sortedlist
         (fold1
            (lambda (entry resu)
               (let ((len (length entry))
                     (toi (cadr entry)))
                  (let lp ((iter resu))
                     (if (null? iter)
                        (list entry)
                        (let ((head (car iter)))
                           (if (or (and (cadr head)
                                        (not toi))
                                   (and (eqv? (not toi) (not (cadr head)))
                                        (< len (length head))))
                              (cons entry iter)
                              (cons head (lp (cdr iter)))))))))
            '()
            rawlist)))
         (let loop ((iter sortedlist))
            (when (pair? iter)
               (let* ((next (cdr iter))
                      (head (car iter))
                      (usym (car head))
                      (tois (cadr head))
                      (istl (cddr head)))
                  (if (and tois (null? next) (< 1 (length istl)))
                     (proc 's usym tois)
                     (for-each
                        (lambda (ista)
                           (proc ista usym tois))
                        istl))
                  (loop next)))))))

(define (show-red-sred bausta auto rule preds sred? capt?)
   (let* ((grammar (auto-grammar auto))
          (idxsta  (bausta-number bausta))
          (symbol  (rule-name rule))
          (pattern (rule-pattern rule))
          (product (rule-product rule))
          (capture (rule-capt rule))
          (lenpat  (length pattern))
          (short?  (= lenpat 1))
          (npred   (if sred? (- lenpat 1) lenpat))
          (ignst   (do ((i (- npred 1) (- i 1))
                        (s ""          (string-append s "_ ")))
                     ((<= i 0) s)))
          (tosts   (pred-set idxsta preds npred)))

      (for-compacted-reduces auto symbol tosts
         (lambda (ista usymbol toista)
               (display "      ((_ ")
               ; show the pattern expression
               ; when there is capture, the first item is matched
               ; note that first item is not valid in absolute
               ; but only valid on my example where captured
               ; items stand single in rules
               (if capt?
                  (begin
                     (display "(")
                     (display (bausta-rule-symbol bausta grammar rule))
                     (display " E ...)"))
                  (display "E"))
               (display " ")
               ; show the predecessor match if required
               (if (positive? npred)
                  (begin
                     (display " (")
                     (display ignst)
                     (display ista)
                     (display " S ...)"))
                  (display "S"))
               (display " ")
               ; show the stack capture if required
               (if (or capt? short?)
                  (display "T")
                  (begin
                     (display "(")
                     (for-each
                        (lambda (c) (display c)(display " "))
                        capture)
                     (display "T ...)")))
               ; show transformation
               (display ")\t(state-")
               (if toista
                  (display toista)
                  (begin
                     (display ista)
                     (display "-")
                     (display usymbol)))
               (if capt?
                  (display " (E ...) ")
                  (display " E "))
               (if toista
                  (begin
                     (display "(")
                     (display ista)
                     (if (positive? npred)
                        (display " S ...) ")
                        (display " . S) ")))
                  (if (positive? npred)
                     (display "(S ...) ")
                     (display "S ")))
               (cond
                  (capt?
                     (display "(")
                     (display product)
                     (display " . T)"))
                  (short?
                     (display "T"))
                  (else
                     (display "(")
                     (display product)
                     (display " T ...)")))
               (display "))")
               (newline)))))

; unalias the shift reduce for 'symbol' of state 'ista' in 'auto'
; and resolve if it finally shift to an other state
; return the pair (aliased-symbol target-ista)
; where aliased-symbol is the symbol after unaliasing
; and target-ista is the target state of the final shift if any
; or #f when there is no final shift
(define (resolve-sred auto ista symbol)
   (let* ((bausta  (vector-ref (auto-states auto) ista))
          (symbol2 (resolve-sred-aliasing bausta symbol))
          (shift   (bausta-find-shift bausta symbol2)))
      (values symbol2 (and shift (bausta-number (shift-target shift))))))

; follows alias shifreds for the same state 'bausta'
; starting with 'symbol'
; returns the final symbol after dealiasing
(define (resolve-sred-aliasing bausta symbol)
   (let ((sred   (bausta-find-shifred bausta symbol)))
      (if (not (and sred (sred-alias? sred)))
         symbol
         (resolve-sred-aliasing bausta (rule-name (sred-rule sred))))))

; test if the shifred 'sred' is an alias
; return the aliased symbol if its the case
; otherwise, return #f
(define (sred-alias? sred)
   (let* ((rule (sred-rule sred))
          (pat  (rule-pattern rule))
          (prod (rule-product rule)))
      (and (= 1 (length pat))
           (equal? prod '$1)
           (rule-name rule))))

;=======================================

(define G (make-grammar GRAMMAR))

;(display-grammar G)

(define A (build-bauto-lr0 G))

;(display-auto A)
(newline)

;(show-automaton A)
(newline)

(simplify-auto A)
;(display-auto A)
(newline)

(show-automaton A)
(newline)
(exit)

;; vim: noai sts=3 ts=3 sw=3 et
