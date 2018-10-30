; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2018
; In the public domain
;
; (disassemble* procedure)  ==>  list
; (disassemble procedure)   ==>  unspecific
;
; (load-from-library "disassemble.scm")
;
; DISASSEMBLE disassembles the bytecode of the compilation
; unit containing the definition of the given PROCEDURE.
; The compilation block typically contains not only the
; code for the procedure itself, but also instructions for
; jumping over the body of the procecure, setting up its
; closure, and binding it to a location in the environment.
; When multiple procedures are defined in the same BEGIN
; block, all procedures contained in the block will be
; disassembled.
;
; The only specification of the Scheme 9 bytecode at the
; present moment is the source code of the compiler and
; abstract machine, so the output of these procedures will
; only make sense to those who are familiar with the
; internal mechanisms of Scheme 9. An incomplete summary
; can be found below.
;
; DISASSEMBLE* returns a list containing instructions and
; DISASSEMBLE pretty-prints code to the current output port.
;
; Scheme 9 Abstract Machine Summary
;
; The Scheme 9 abstract machine is an SECD-like machine with
; a unified return and data stack, a top of stack cache called
; the "accumulator", and vectors as environments.
;
; Single-operand instructions expect their arguments in the
; accumulator (A), two-operand instructions expect argument #1 in
; the accumulator and asrgument #2 on the top of the stack (S0),
; and three-argument instructions expect #1 in A, #2 in S0 and
; #3 in S1 (the stack element beneath S0). All instructions
; remove their operands from the stack and return their result
; in A.
;
; Many instructions resemble Scheme procedures. For instance,
; the OP:CONS instruction implements CONS and the OP_SUBSTRING
; instruction implements SUBSTRING. There are no variadic
; instructions. The expression (+ a b c), for instance, would
; compile to  A PUSH B PLUS PUSH C PLUS.
;
; Here is a summary of the instructions that DO NOT resemble
; built-in Scheme procedures:
;
; OP:APPLIS       |  A=procedure S0=list  -->  object
; OP:TAIL-APPLIS  |  A=procedure S0=list  -->  object
; Apply A to arguments in S0.
; OP:TAIL-APPLIS performs a tail call.
;
; OP:APPLY      |  A=procedure S0...Sn=objects  -->  object
; OP:TAILAPPLY  |  A=procedure S0...Sn=objects  -->  object
; Apply A to arguments on the stack.
; OP:TAIL-APPLY performs a tail call.
;
; OP:QUOTE x  |  -->  object
; Load A with x.
;
; OP:ARG n  |  -->  object
; Retrieve N'th procedure argument.
;
; OP:REF n sym  |  -->  object
; Retrieve the value from the N'th environment slot.
; SYM is the name of the symbol bound to the slot.
;
; OP:MAKE-ENV  n  |  -->  vector
; OP:PROP-ENV     |  -->  vector
; Create an empty environment vector with N slots.
; OP:PROP-ENV propagates (re-uses) the parent environment.
;
; OP:COPY-ARG n m  |  -->
; Copy the N'th procedure argument to slot M of the new
; environment.
;
; OP:COPY-REF  |  n m  -->
; Copy envirnment slot N of the parent environment to
; slot M of the new environment.
;
; OP:CLOSURE n  |  -->  procedure
; Create a procedure (closure) with entry point N. Fetch
; the new environment from A.
;
; OP:DEF-MACRO sym  |  procedure  -->
; Bind A to SYM in the macro symbol table.
;
; OP:ENTER n       |  -->
; Enter procedure. When there are not exactly N arguments on
; the stack, signal an error.
;
; OP:ENTER-COLL n  |  -->
; Enter procedure collecting arguments. When there are less
; than n arguments on the stack, signal an error. Collect
; excess arguments (>N) in a list and place them in the last
; argument.
;
; OP:RETURN  |  -->
; Return from procedure.
;
; OP:HALT  |  -->
; Halt program execution.
;
; OP:JMP k  |  -->
; Transfer control to address K.
;
; OP:JMP-FALSE  |  boolean  -->
; Transfer control to address K, if A equals #F.
;
; OP:JMP-TRUE  |  boolean -->
; Transfer control to address K, if A does not equal #F.
;
; OP:POP  |  -->  object
; Load S0 into A.
;
; OP:PUSH  |  object  -->
; Push A to the stack.
;
; OP:PUSH-VAL n  |  -->  object
; Push N to the stack.
;
; OP:SET-ARG n  |  object  -->
; Set the N'th argument to A.
;
; OP:SET-REF n  |  object  -->
; Set the N'th environment slot to A.
;
; (Example:) No example given.

(load-from-library "iota.scm")

(define-syntax (enum syms . body)
  (let* ((k  (length syms))
         (ns (iota 0 (- k 1))))
    `((lambda ,syms . ,body) . ,ns)))

(define (disassemble* p)
  (enum (op:applis op:apply op:arg op:copy-arg op:closure op:copy-ref
         op:def-macro op:enter op:enter-coll op:halt op:jmp op:jmp-false
         op:jmp-true op:make-env op:prop:env op:pop op:push op:push-val
         op:quote op:ref op:return op:set-arg op:set-ref
         op:tail-applis op:tail-apply op:abs op:append op:assq
         op:assv op:bit-op op:boolean-p op:caaaar op:caaadr op:caaar
         op:caadar op:caaddr op:caadr op:caar op:cadaar op:cadadr
         op:cadar op:caddar op:cadddr op:caddr op:cadr op:call-cc op:car
         op:catch op:catch-tag-p op:cdaaar op:cdaadr op:cdaar op:cdadar
         op:cdaddr op:cdadr op:cdar op:cddaar op:cddadr op:cddar
         op:cdddar op:cddddr op:cdddr op:cddr op:cdr op:ceiling
         op:char-alphabetic-p op:char-ci-equal-p op:char-ci-grtr-p
         op:char-ci-gteq-p op:char-ci-less-p op:char-ci-lteq-p
         op:char-downcase op:char-equal-p op:char-grtr-p op:char-gteq-p
         op:char-less-p op:char-lower-case-p op:char-lteq-p
         op:char-numeric-p op:char-p op:char-to-integer op:char-upcase
         op:char-upper-case-p op:char-whitespace-p op:close-input-port
         op:close-output-port op:command-line op:cons
         op:current-error-port op:current-input-port
         op:current-output-port op:delete-file op:display op:divide
         op:dump-image op:environment-variable op:eof-object-p op:equal
         op:eqv-p op:eq-p op:error op:error2 op:eval op:even-p
         op:exact-p op:exact-to-inexact op:exponent op:expt
         op:file-exists-p op:fix-exactness op:floor op:gensym op:grtr
         op:gteq op:inexact-p op:inexact-to-exact op:input-port-p
         op:integer-p op:integer-to-char op:length op:less op:list
         op:list-ref op:list-tail op:list-to-string op:list-to-vector
         op:load op:lteq op:macro-expand op:macro-expand-1
         op:make-string op:make-vector op:mantissa op:max op:memq
         op:memv op:min op:minus op:negate op:negative-p op:not
         op:null-p op:odd-p op:open-append-file op:open-input-file
         op:open-output-file op:output-port-p op:pair-p op:peek-char
         op:plus op:positive-p op:procedure-p op:quit op:quotient
         op:read op:read-char op:real-p op:remainder op:reverse
         op:reverse-b op:s9-bytecode op:set-car-b op:set-cdr-b
         op:set-input-port-b op:set-output-port-b op:stats
         op:string-append op:string-copy op:string-equal-p
         op:string-fill-b op:string-grtr-p op:string-gteq-p
         op:string-length op:string-less-p op:string-lteq-p op:string-p
         op:string-ref op:string-set-b op:string-ci-equal-p
         op:string-ci-grtr-p op:string-ci-gteq-p op:string-ci-less-p
         op:string-ci-lteq-p op:string-to-list op:string-to-symbol
         op:substring op:symbols op:symbol-p op:symbol-to-string
         op:system-command op:throw op:times op:truncate op:vector
         op:vector-append op:vector-copy op:vector-fill-b
         op:vector-length op:vector-p op:vector-ref op:vector-set-b
         op:vector-to-list op:write op:write-char op:zero-p)

    (define mnemos
      #(applis apply arg copy-arg closure copy-ref def-macro enter
        enter-coll halt jmp jmp-false jmp-true make-env prop-env pop
        push push-val quote ref return set-arg set-ref tail-applis
        tail-apply abs append assq assv bit-op boolean? caaaar caaadr
        caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr
        caddr cadr call/cc car catch catch-tag? cdaaar cdaadr cdaar
        cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
        cddr cdr ceiling char-alphabetic? char-ci=? char-ci>?
        char-ci>=? char-ci<? char-ci<=? char-downcase char=? char>?
        char>=? char<? char-lower-case? char<=? char-numeric? char?
        char->integer char-upcase char-upper-case? char-whitespace?
        close-input-port close-output-port command-line cons
        current-error-port current-input-port current-output-port
        delete-file display divide dump-image environment-variable
        eof-object? equal eqv? eq? error error2 eval even? exact?
        exact->inexact exponent expt file-exists? fix-exactness floor
        gensym grtr gteq inexact? inexact->exact input-port? integer?
        integer->char length less list list-ref list-tail list->string
        list->vector load lteq macro-expand macro-expand-1 make-string
        make-vector mantissa max memq memv min minus negate negative?
        not null? odd? open-append-file open-input-file open-output-file
        output-port? pair? peek-char plus positive? procedure? quit
        quotient read read-char real? remainder reverse reverse-b
        s9:bytecode set-car! set-cdr! set-input-port! set-output-port!
        stats string-append string-copy string=? string-fill! string>?
        string>=? string-length string<? string<=? string? string-ref
        string-set! string-ci=? string-ci>? string-ci>=? string-ci<?
        string-ci<=? string->list string->symbol substring symbols
        symbol? symbol->string system-command throw times truncate
        vector vector-append vector-copy vector-fill! vector-length
        vector? vector-ref vector-set! vector->list write write-char
        zero?))

    (define group2 (list op:quote op:arg op:push-val op:jmp
                         op:jmp-false op:jmp-true op:make-env op:closure
                         op:enter op:enter-coll op:set-arg op:set-ref
                         op:def-macro))

    (define group3 (list op:ref op:copy-arg op:copy-ref))

    (define (mnemo op) (vector-ref mnemos op))

    (let loop ((bc  (vector->list (s9:bytecode p)))
               (dis '()))
      (cond ((null? bc)
              (reverse! dis))
            ((memv (car bc) group3)
              (loop (cdddr bc)
                    (cons (list (mnemo (car bc))
                                (cadr bc)
                                (caddr bc))
                          dis)))
            ((memv (car bc) group2)
              (loop (cddr bc)
                    (cons (list (mnemo (car bc))
                                (cadr bc))
                          dis)))
            (else
              (loop (cdr bc)
                    (cons (list (mnemo (car bc)))
                          dis)))))))

(define (disassemble p)

  (define (numlen x)
    (string-length (number->string x)))

  (define (symlen x)
    (string-length (symbol->string x)))

  (define (spaces n)
    (cond ((positive? n)
            (display #\space)
            (spaces (- n 1)))))

  (let* ((d (disassemble* p))
         (k (+ 1 (fold-left max 0 (map symlen (map car d)))))
         (a 0))
    (for-each
      (lambda (x)
        (spaces (- 5 (numlen a)))
        (display a)
        (display #\space)
        (display (car x))
        (cond ((pair? (cdr x))
                (spaces (- k (symlen (car x))))
                (write (cadr x))
                (cond ((pair? (cddr x))
                        (display #\space)
                        (write (caddr x))))))
        (newline)
        (set! a (+ a (length x))))
      d)))
