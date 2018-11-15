;
(require-extension sys-plan9)

(define (pwd)
  (let* ((fd (sys:open "." 0)) (cwd (sys:fd->path fd)))
    (sys:close fd)
    cwd))

(define (! x)
  (let ((a (string-split #\space x)))
    (if (not (eq? #\/ (string-ref (car a) 0)))
      (set-car! a (string-append "/bin/" (car a))))
    (let ((pid (sys:fork)))
      (cond ((zero? pid) (sys:exec (car a) (cdr a)))
            ((> pid 0) (sys:await))))))

(define (getenv x)
  (if (symbol? x) (set! x (symbol->string x)))
  (let* ((fd (sys:open (string-append "/env/" x) 0))
         (val (sys:read fd 1000)))
    (sys:close fd)
    val))

;
; Useful during developing
;
(define (remake)
  (and (! "mk all") (sys:exec "./s9" '())))
