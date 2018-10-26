; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2007-2018
; In the public domain
;
; (syntax-rules (<keyword> ...) <rule> ...)  ==>  procedure
;
; See the R4RS Appendix for details.
;
; Caveat: Patterns *must* begin with the symbol "_".
;
; Caveat2: Only standard-Scheme binding constructs (LAMBDA, LET
;          LET*, LETREC, DO) will be protected from variable
;          capturing.

(load-from-library "flatten.scm")
(load-from-library "filter.scm")
(load-from-library "intersection.scm")

; Match FORM against PATTERN; KEYWORDS contains the keywords
; of SYNTAX-RULES.
; When the given form matches the pattern, bind each variable
; of PATTERN to the corresponding part of the FORM and return
; an environment containing the new bindings. In case of a
; mismatch, return #F.
; Ellipses will generate nested environments.

(define (syntax-match form pat kws)

  (define (match form pat kws env)
    (cond ((pair? pat)
            (cond
              ((and (pair? (cdr pat))
                    (eq? '... (cadr pat))
                    (or (pair? form)
                        (null? form)))
                (let ((e* (map (lambda (x)
                                 (match x (car pat) kws '()))
                               form)))
                  (if (memq #f e*)
                      #f
                      (cons e* env))))
              ((pair? form)
                (let ((e (match (car form) (car pat) kws env)))
                  (and e (match (cdr form) (cdr pat) kws e))))
              (else
                #f)))
          ((memq pat kws)
            (if (eq? pat form) env #f))
          ((symbol? pat)
            (cons (cons pat form) env))
          (else
            (if (eq? pat form) env #f))))

  (let ((e (match form pat kws '())))
    (if e (reverse! e) e)))

; Give a unique name to each variable that is bound in FORM.
; BOUND is a list of initially bound variables. This function
; also renames variables of (named) LET, LET*, LETREC, and DO,
; e.g.:
;
; (ALPHA-CONV '(LET ((X Y)) X) '()) => (LET ((G123 Y)) G123)

(define (alpha-conv form bound)

  (define (subst x env)
    (cond ((assq x env) => cdr)
          (else x)))

  (define (map-improper f a r)
    (cond ((null? a)
            (reverse! r))
          ((not (pair? a))
            (append (reverse! r) (f a)))
          (else
            (map-improper f (cdr a) (cons (f (car a)) r)))))

  (define (remove-bound env bound)
    (cond ((null? env)
            '())
          ((memq (caar env) bound)
            (remove-bound (cdr env) bound))
          (else
            (cons (car env)
                  (remove-bound (cdr env) bound)))))

  (define (conv form env)
    (cond ((symbol? form)
            (subst form env))
          ((not (pair? form))
            form)
          ((and (eq? 'quote (car form))
                (pair? (cdr form))
                (null? (cddr form)))
            form)
          ((and (eq? 'lambda (car form))
                (pair? (cdr form))
                (pair? (cddr form)))
            (let ((e (map-improper (lambda (x)
                                     (cons x (gensym)))
                                   (flatten (cadr form))
                                   '())))
              `(lambda ,@(conv (cdr form)
                               (append (remove-bound e bound)
                                       env)))))
          ((and (eq? (car form) 'let)
                (pair? (cdr form))
                (symbol? (cadr form))
                (pair? (cddr form)))
            (let* ((e (list (cons (cadr form) (gensym))))
                   (x (conv `(let ,@(cddr form))
                             (append e env))))
              `(let ,(cdar e) ,@(cdr x))))
          ((and (or (eq? (car form) 'let)
                    (eq? (car form) 'letrec)
                    (eq? (car form) 'let*)
                    (eq? (car form) 'do))
                (pair? (cdr form))
                (pair? (cadr form))
                (pair? (caadr form))
                (pair? (cddr form)))
            (let ((e (map-improper (lambda (x)
                                     (cons x (gensym)))
                                   (map (lambda (x)
                                          (if (pair? x) (car x) #f))
                                        (cadr form))
                                   '())))
              `(,(car form) ,@(conv (cdr form)
                                    (append (remove-bound e bound)
                                            env)))))
          (else
            (map-improper (lambda (x) (conv x env))
                          form
                          '()))))

  (conv form '()))

; Substitute variables of TMPL by values of ENV.

(define syntax-expand
  (let ((alpha-conv alpha-conv))
    (lambda (bound tmpl env)

      (define (atom-env env)
        (filter (lambda (x)
                  (and (pair? x) (symbol? (car x))))
                env))

      (define (ell-env env)
        (let* ((e  (filter (lambda (x)
                             (and (pair? x) (pair? (car x))))
                           env))
               (e  (map (lambda (x)
                          (apply map list x))
                        e))
               (e  (apply append e))
               (vs (map caar e))
               (as (map (lambda (x) (reverse! (map cdr x))) e))
               (e  (map cons vs as)))
          e))

      (define (map-cons a b)
        (cond ((null? a) '())
              ((null? b)
                (cons (cons (car a) '())
                      (map-cons (cdr a) '())))
              (else
                (cons (cons (car a) (car b))
                      (map-cons (cdr a) (cdr b))))))

      (define (map-all f a)
        (cond ((null? a) '())
              ((null? (car a))
                (map-all f (cdr a)))
              (else
                (cons (f (car a))
                      (map-all f (cdr a))))))

      (define (next-args vs as)
        (let loop ((vs vs)
                   (as as)
                   (nv '())
                   (na '()))
          (cond ((null? vs) (list (reverse! nv)
                                  (reverse! na)))
                ((null? (cdar as))
                  (loop (cdr vs) (cdr as) nv na))
                (else
                  (loop (cdr vs)
                        (cdr as)
                        (cons (car vs) nv)
                        (cons (cdar as) na))))))

      (define (expand-ellipsis tmpl eenv)
        (let ((tvs (flatten tmpl)))
          (let loop ((vs (map car eenv))
                     (as (map cdr eenv))
                     (r  '()))
            (if (null? (intersection vs tvs))
                r
                (let* ((b  (map-cons vs (map-all car as)))
                       (n  (next-args vs as))
                       (vs (car n))
                       (as (cadr n)))
                  (loop vs
                        as
                        (cons (expand tmpl b '()) r)))))))

      (define (expand tmpl env eenv)
        (cond
          ((and (pair? tmpl)
                (pair? (cdr tmpl))
                (eq? (cadr tmpl) '...))
            (append (expand-ellipsis (car tmpl) eenv)
                    (cddr tmpl)))
          ((pair? tmpl)
            (cons (expand (car tmpl) env eenv)
                  (expand (cdr tmpl) env eenv)))
          ((assq tmpl env) => cdr)
          (else tmpl)))

      (expand (alpha-conv tmpl bound)
              (atom-env env)
              (ell-env env)))))

; Check the syntax of SYNTAX-RULES and rewrite it
; to a LAMBDA expression.

(define-syntax (syntax-rules kws . rules)

  (define (list-of? p a)
    (or (null? a)
        (and (p (car a))
             (list-of? p (cdr a)))))

  (define (keywords-ok? x)
         (list-of? symbol? x))

  (define (rules-ok? x)
    (list-of? (lambda (x)
                (and (pair? x)
                     (pair? (car x))
                     (pair? (cdr x))
                     (null? (cddr x))))
              x))

  (define pattern caar)

  (define template cadar)

  (define (rewrite-rules app kws rules-in rules-out)
    (if (null? rules-in)
        (reverse! rules-out)
        (rewrite-rules
          app
          kws
          (cdr rules-in)
          (cons `((syntax-match ,app
                                ',(pattern rules-in)
                                ',kws)
                   => (lambda (env)
                        (syntax-expand ',(flatten (pattern rules-in))
                                       ',(template rules-in)
                                       env)))
                rules-out))))

  (cond
    ((null? rules)
      (error "syntax-rules: too few arguments" rules))
    ((not (keywords-ok? kws))
      (error "syntax-rules: malformed keyword list" kws))
    ((not (rules-ok? rules))
      (error "syntax-rules: invalid clause in rules" rules))
    (else
      (let* ((app (gensym))
             (default `((else (error "syntax error" ,app)))))
        `(lambda ,app
           (let ((,app (cons '_ ,app)))
             (cond ,@(rewrite-rules app kws rules '())
                   ,@default)))))))
