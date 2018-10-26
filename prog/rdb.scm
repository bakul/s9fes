#!/u/bin/s9

; Simple CSV database tool
; By Nils M Holm, 2017, 2018
;
; In the public domain.
; Alternatively: provided under the CC0 license
; https://creativecommons.org/publicdomain/zero/1.0/
;
; Run "rdb help" for help

(require-extension csv)

(load-from-library "read-line.scm")
(load-from-library "string-case.scm")
(load-from-library "string-find.scm")
(load-from-library "string-split.scm")
(load-from-library "position.scm")
(load-from-library "mergesort.scm")
(load-from-library "for-all.scm")
(load-from-library "filter.scm")
(load-from-library "hash-table.scm")
(load-from-library "intersection.scm")
(load-from-library "set-difference.scm")

(define (read-record)
  (define (str s)
    (list->string (reverse s)))

  (let ((s (read-line)))
    (if (eof-object? s)
        s
        (let cvt ((s (string->list s))
                  (f #f)
                  (r '()))
          (cond ((null? s)
                  (if f
                      (reverse (cons (str f) r))
                      (reverse r)))
                ((char=? #\" (car s))
                  (if (eqv? #\" (and (pair? (cdr s)) (cadr s)))
                      (if f
                          (cvt (cddr s) (cons (car s) f) r)
                          (cvt (cddr s) #f (cons "" r)))
                      (if f
                          (cvt (cdr s) #f (cons (str f) r))
                          (cvt (cdr s) '() r))))
                (else
                  (if f
                      (cvt (cdr s) (cons (car s) f) r)
                      (cvt (cdr s) f r))))))))

(define read-record csv:read)

(define (qprint s)
  (let* ((k (string-length s))
         (t (make-string (* 2 k))))
    (let loop ((i 0)
               (j 0))
      (cond ((= i k)
              (display (substring t 0 j)))
            ((char=? #\" (string-ref s i))
              (string-set! t j #\")
              (string-set! t (+ 1 j) #\")
              (loop (+ 1 i)
                    (+ 2 j)))
            (else
              (string-set! t j (string-ref s i))
              (loop (+ 1 i)
                    (+ 1 j)))))))

(define (prin-record rec ps)
  (display "\"")
  (for-each (lambda (x)
              (qprint (list-ref rec (car x)))
              (if (cdr x)
                  (display "\",\"")))
            ps)
  (display "\""))

(define (prin-record rec ps)
  (let ((v (list->vector rec)))
    (csv:write (map (lambda (x) (vector-ref v (car x)))
                    ps))))

(define (print-record rec ps)
  (prin-record rec ps)
  (newline))

(define (get-attrs)
  (map string-downcase (read-record)))

(define (empty-rec ps)
  (map (lambda (x) "") ps))

(define (get-indices ns names)
  (let* ((ps (map (lambda (x) (position x names)) ns))
         (ps (begin (for-each (lambda (x)
                                (if (not (position x names))
                                    (error "no such field" x)))
                              ns)
                    ps))
         (is (map (lambda (x) #t) (iota (length ns))))
         (is (reverse (cons #f (cdr is)))))
    (map cons ps is)))
  
(define (extract ns del)
  (let* ((names (get-attrs))
         (_ (get-indices ns names))
         (ns (if del
                 (set-difference names ns)
                 ns))
         (ps (get-indices ns names)))
      (print-record names ps)
      (let report ((rec (read-record)))
        (if (not (eof-object? rec))
            (begin (print-record rec ps)
                   (report (read-record)))))))

(define (insert ns)
  (let* ((names (get-attrs))
         (p* (get-indices names names))
         (ns (map (lambda (x) (string-split #\= x)) ns))
         (vs (map cadr ns))
         (ns (map car ns))
         (ps (map car (get-indices ns names)))
         (nr (empty-rec p*)))
      (print-record names p*)
      (let copy ((rec (read-record)))
        (if (not (eof-object? rec))
            (begin (print-record rec p*)
                   (copy (read-record)))))
      (map (lambda (p v)
             (set-car! (list-tail nr p) v))
           ps vs)
      (print-record nr p*)))

(define (inscol ns)
  (let* ((names (get-attrs))
         (a (string-split #\= ns))
         (nf (car a))
         (v (if (pair? (cdr a))
                (list (cadr a))
                '("")))
         (names (append names (list nf)))
         (ps (get-indices names names)))
      (print-record names ps)
      (let add ((rec (read-record)))
        (if (not (eof-object? rec))
            (let ((rec (append rec v)))
              (print-record rec ps)
              (add (read-record)))))))

(define (string-value s)
  (if (string=? "" s)
      0
      (string->number s)))

(define (sum-field n)
  (let* ((names (get-attrs))
         (p (caar (get-indices (list n) names))))
      (let report ((rec (read-record))
                   (k   0))
        (if (eof-object? rec)
            (print k)
            (report (read-record)
                    (+ k (string-value
                           (list-ref rec p))))))))

(define (extract-if n s ns rem cont)
  (let* ((names (get-attrs))
         (ns (if (null? ns) names ns))
         (ps (get-indices ns names))
         (tp (position n names)))
      (if (not tp)
          (error "no such field" n))
      (print-record names ps)
      (let report ((rec (read-record)))
        (cond ((eof-object? rec))
              ((and cont (string-ci-find s (list-ref rec tp)))
                (if (not rem) (print-record rec ps))
                (report (read-record)))
              ((and (not cont) (string-ci=? s (list-ref rec tp)))
                (if (not rem) (print-record rec ps))
                (report (read-record)))
              (else
                (if rem (print-record rec ps))
                (report (read-record)))))))

(define (collate ns)
  (let* ((fn (if (null? (cdr ns))
                 "count"
                 (cadr ns)))
         (ns (list (car ns)))
         (names (get-attrs))
         (ps (get-indices ns names))
         (tp (position (car ns) names))
         (ht (make-hash-table)))
      (if (not tp)
          (error "no such field" n))
      (print-record `(,(car ns) ,fn) '((0 . #t) (1 . #f)))
      (let coll ((rec (read-record)))
        (cond ((eof-object? rec)
                (for-each
                  (lambda (x)
                    (print-record x '((0 . #t) (1 . #f))))
                  (map (lambda (x)
                         (list (car x) (number->string (cdr x))))
                       (hash-table->alist ht))))
              ((hash-table-ref ht (list-ref rec tp))
                => (lambda (v)
                     (hash-table-set!
                       ht
                       (list-ref rec tp)
                       (+ 1 (car v)))
                     (coll (read-record))))
              (else
                (hash-table-set! ht (list-ref rec tp) 1)
                (coll (read-record)))))))

(define (collect ns attr)
  (define (select r ps)
    (map (lambda (k)
           (list-ref r k))
         ps))

  (let* ((names (if attr attr (get-attrs)))
         (ns    (if (null? ns) names ns))
         (ps    (map car (get-indices ns names))))
      (let collect ((rec  (read-record))
                    (rec* (list names)))
        (if (eof-object? rec)
            (map (lambda (x)
                   (select x ps))
                 (reverse rec*))
            (collect (read-record)
                     (cons rec rec*))))))

(define (fields)
  (let* ((names (get-attrs))
         (ps    (get-indices names names)))
    (print-record '("name") '((0 . #f)))
    (for-each (lambda (x) (print-record (list x) '((0 . #f))))
              names)))

(define (sort ns rev)
  (let* ((rec* (collect '() #f))
         (names (car rec*))
         (rec* (cdr rec*))
         (ps (get-indices names names))
         (tp (position (car ns) names)))
      (if (not tp)
          (error "no such field" (car ns)))
      (print-record names ps)
      (let* ((pred (if (for-all string-value
                               (map (lambda (x)
                                      (list-ref x tp))
                                    rec*))
                      (lambda (x y)
                       (<= (string-value (list-ref x tp))
                           (string-value (list-ref y tp))))
                      (lambda (x y)
                       (string<=? (list-ref x tp)
                                  (list-ref y tp)))))
            (pred (if rev (lambda (x y) (not (pred x y))) pred)))
        (for-each
          (lambda (x) (print-record x ps))
          (mergesort pred rec*)))))

(define (join file left right)
  (define (make-ht r* k)
    (let ((h (make-hash-table)))
      (map (lambda (x)
             (hash-table-set! h (list-ref x k) x))
           r*)
      h))

  (define (prin-key x)
    (display "\"")
    (qprint x)
    (display "\","))

  (let* ((inner (not (or left right)))
         (rec* (collect '() #f))
         (nl (car rec*))
         (rec* (cdr rec*))
         (rec2* (with-input-from-file
                  file
                  (lambda ()
                    (collect '() #f))))
         (nl2 (car rec2*))
         (rec2* (cdr rec2*))
         (n (intersection nl nl2))
         (n (if (= 1 (length n))
                (car n)
                (error "tables not compatible in join")))
         (nm (filter (lambda (x) (not (string=? x n)))
                     nl))
         (ps (get-indices nm nl))
         (tp (position n nl))
         (ht (make-ht rec* tp))
         (tp2 (position n nl2))
         (nm2 (filter (lambda (x) (not (string=? x n)))
                      nl2))
         (ps2 (get-indices nm2 nl2))
         (ht2 (make-ht rec2* tp2))
         (htt (make-hash-table)))
    (if (not tp)
        (error "no such field" n))
    (if (not tp2)
        (error "no common key in join" file))
    (prin-key n)
    (prin-record nl ps)
    (display ",")
    (print-record nl2 ps2)
    (if inner
        (for-each
          (lambda (x)
            (cond ((hash-table-ref ht2 (list-ref x tp))
                    => (lambda (v)
                         (prin-key (list-ref x tp))
                         (prin-record x ps)
                         (display ",")
                         (print-record (car v) ps2)))))
          rec*))
    (if left
        (for-each
          (lambda (x)
            (prin-key (list-ref x tp))
            (prin-record x ps)
            (display ",")
            (cond ((hash-table-ref ht2 (list-ref x tp))
                    => (lambda (v)
                         (hash-table-set! htt (list-ref x tp) (car v))
                         (hash-table-remove! ht2 (list-ref x tp))
                         (print-record (car v) ps2)))
                  ((hash-table-ref htt (list-ref x tp))
                    => (lambda (v)
                         (print-record (car v) ps2)))
                  (else
                    (print-record (empty-rec nl2) ps2))))
          rec*))
    (if right
        (for-each
          (lambda (x)
            (prin-key (list-ref x tp2))
            (cond ((hash-table-ref ht (list-ref x tp2))
                    => (lambda (v)
                         (hash-table-remove! ht (list-ref x tp2))
                         (prin-record (car v) ps)))
                  (else
                    (prin-record (empty-rec nl) ps)))
            (display ",")
            (print-record (cdr x) ps2))
          (hash-table->alist ht2)))))

(define (pprint ns)
  (define (pad s k c e)
    (string-append
      (if (string->number s)
          (string-append
            (make-string (- k (string-length s)) c)
            s)
          (string-append
            s
            (make-string (- k (string-length s))
                         c)))
      e))

  (define (lengths ns)
    (map (lambda (s)
           (let ((x (string-split #\: s)))
             (if (> (length x) 1)
                 (string->number (cadr x))
                 0)))
         ns))

  (define (names ns)
    (map (lambda (s)
           (let ((x (string-split #\: s)))
             (if (> (length x) 1)
                 (car x)
                 s)))
         ns))

  (define (trim k r)
    (map (lambda (x)
            (map (lambda (k r)
                   (if (or (zero? k) (>= k (string-length r)))
                       r
                       (substring r 0 k)))
                 k x))
         r))

  (let* ((at (get-attrs))
         (ns (if (null? ns) at (names ns)))
         (ks (lengths ns))
         (r  (collect ns at))
         (h  (map string-upcase (car r)))
         (r+ (apply map list r))
         (r  (cdr r))
         (k  (map (lambda (c)
                    (apply max (map string-length c)))
                  r+))
         (k  (map (lambda (x lim)
                    (if (and (not (zero? lim))
                             (< lim x))
                        lim
                        x))
                  k ks))
         (ln (map (lambda (k) (pad "" k #\- "")) k))
         (r  (trim ks (cons h (cons ln r)))))
    (for-each
      (lambda (r)
        (for-each
          (lambda (f k)
            (display (pad f k #\space " ")))
          r k)
        (newline))
      r)))

(define (help)
  (for-each
    (lambda (x) (display x) (newline))
    '(""
      "Usage: rdb command"
      ""
      "commands:"
      "coll c          collate values of column c"
      "col c ...       extract columns"
      "del c ...       delete columns"
      "insert c=v ...  insert new row with given columns/values"
      "inscol c[=v]    insert new column with optional value"
      "join file       full-join file (union)"
      "joini file      inner-join file (intersection)"
      "joinl file      left-join file"
      "joinr file      right-join file"
      "names           extract column names"
      "print c ...     pretty-print columns, c:n limits length to n"
      "except c~s      extract rows where column doesn't contain string"
      "except c=s      extract rows where column doesn't equal string"
      "rsort c         sort rows by column, descending order"
      "sort c          sort rows by column, ascending order"
      "sum c           pretty-print sum of numeric column"
      "where c~s       extract rows where column contains string"
      "where c=s       extract rows where column equals string"
      "")))

(define (assert-args! l h a*)
  (let ((k (length a*)))
    (if (or (< k l)
            (and h (> k h)))
        (error "wrong number of arguments"))))

(let ((a* *arguments*))
  (let ((splitarg
          (lambda (x)
            (let* ((x1 (string-split #\= x))
                   (x2 (string-split #\~ x))
                   (c  (null? (cdr x1)))
                   (f  (if c (car x2) (car x1)))
                   (s  (if c (cadr x2) (cadr x1))))
             (list c f s)))))
    (cond ((null? a*)
            (display "type \"rdb help\" for help")
            (newline))
          ((member (car a*) '("test"))
            (read-record))
          ((member (car a*) '("collate" "coll"))
            (assert-args! 1 2 (cdr a*))
            (collate (cdr a*)))
          ((member (car a*) '("columns" "column" "cols" "col"))
            (assert-args! 1 #f (cdr a*))
            (extract (cdr a*) #f))
          ((member (car a*) '("delete-column" "delcol" "del"))
            (assert-args! 1 #f (cdr a*))
            (extract (cdr a*) #t))
          ((member (car a*) '("help" "?"))
            (help))
          ((member (car a*) '("inner-join" "ijoin" "joini"))
            (assert-args! 1 1 (cdr a*))
            (join (cadr a*) #f #f))
          ((member (car a*) '("insert-row" "insert" "ins"))
            (assert-args! 1 #f (cdr a*))
            (insert (cdr a*)))
          ((member (car a*) '("insert-column" "inscol"))
            (assert-args! 1 1 (cdr a*))
            (inscol (cadr a*)))
          ((member (car a*) '("join" "full-join" "union"))
            (assert-args! 1 1 (cdr a*))
            (join (cadr a*) #t #t))
          ((member (car a*) '("left-join" "ljoin" "joinl"))
            (assert-args! 1 1 (cdr a*))
            (join (cadr a*) #t #f))
          ((member (car a*) '("names" "fields"))
            (assert-args! 0 0 (cdr a*))
            (fields))
          ((member (car a*) '("print"))
            (pprint (cdr a*)))
          ((member (car a*) '("reverse-sort" "revsort" "rsort"))
            (assert-args! 1 1 (cdr a*))
            (sort (cdr a*) #t))
          ((member (car a*) '("right-join" "rjoin" "joinr"))
            (assert-args! 1 1 (cdr a*))
            (join (cadr a*) #f #t))
          ((member (car a*) '("sort"))
            (assert-args! 1 1 (cdr a*))
            (sort (cdr a*) #f))
          ((member (car a*) '("sum"))
            (assert-args! 1 1 (cdr a*))
            (sum-field (cadr a*)))
          ((member (car a*) '("where"))
            (assert-args! 1 1 (cdr a*))
            (let ((cfs (splitarg (cadr a*))))
              (extract-if (cadr cfs) (caddr cfs) (cddr a*) #f (car cfs))))
          ((member (car a*) '("except" "exc" "remove" "rem"))
            (assert-args! 1 1 (cdr a*))
            (let ((cfs (splitarg (cadr a*))))
              (extract-if (cadr cfs) (caddr cfs) (cddr a*) #t (car cfs))))
          (else
            (error "unknown command" (car a*))))))
