; Scheme 9 from Empty Space, Function Library
; By Bakul Shah, 2009-2011
;
; An interface to some Plan9 system services.
;
; (sys:dirstat s)
; (sys:dirfstat i)
; (sys:dirwstat s st)
; (sys:dirfwstat i st)
; (is-dir? file)
; (sys:getenv name)
; (sys:putenv name val)

(require-extension sys-plan9)

(load-from-library "bitops")
(load-from-library "define-structure")

(define-structure dir
  type
  dev
  qid
  mode
  atime
  mtime
  length
  name
  uid
  gid
  muid)

(define sys:DMDIR    (sys:magic-const "DMDIR"))
(define sys:DMMOUNT  (sys:magic-const "DMMOUNT"))
(define sys:DMAUTH   (sys:magic-const "DMAUTH"))
(define sys:DMAPPEND (sys:magic-const "DMAPPEND"))
(define sys:DMEXCL   (sys:magic-const "DMEXCL"))
(define sys:DMEXEC   (sys:magic-const "DMEXEC"))
(define sys:DMREAD   (sys:magic-const "DMREAD"))
(define sys:DMTMP    (sys:magic-const "DMTMP"))
(define sys:DMWRITE  (sys:magic-const "DMWRITE"))
(define sys:MORDER   (sys:magic-const "MORDER"))
(define sys:MREPL    (sys:magic-const "MREPL"))
(define sys:MBEFORE  (sys:magic-const "MBEFORE"))
(define sys:MAFTER   (sys:magic-const "MAFTER"))
(define sys:MCREATE  (sys:magic-const "MCREATE"))
(define sys:MCACHE   (sys:magic-const "MCACHE"))
(define sys:STATMAX  (sys:magic-const "STATMAX"))
(define sys:DIRMAX   (sys:magic-const "DIRMAX"))
(define sys:ERRMAX   (sys:magic-const "ERRMAX"))
(define sys:OREAD    (sys:magic-const "OREAD"))
(define sys:OWRITE   (sys:magic-const "OWRITE"))
(define sys:ORDWR    (sys:magic-const "ORDWR"))
(define sys:OEXEC    (sys:magic-const "OEXEC"))
(define sys:OTRUNC   (sys:magic-const "OTRUNC"))
(define sys:OCEXEC   (sys:magic-const "OCEXEC"))
(define sys:ORCLOSE  (sys:magic-const "ORCLOSE"))
(define sys:OEXCL    (sys:magic-const "OEXCL"))
(define sys:NCONT    (sys:magic-const "NCONT"))
(define sys:NDFLT    (sys:magic-const "NDFLT"))
(define sys:NSAVE    (sys:magic-const "NSAVE"))
(define sys:NRSTR    (sys:magic-const "NRSTR"))
(define sys:QTDIR    (sys:magic-const "QTDIR"))
(define sys:QTAPPEND (sys:magic-const "QTAPPEND"))
(define sys:QTEXCL   (sys:magic-const "QTEXCL"))
(define sys:QTMOUNT  (sys:magic-const "QTMOUNT"))
(define sys:QTAUTH   (sys:magic-const "QTAUTH"))
(define sys:QTTMP    (sys:magic-const "QTTMP"))
(define sys:QTFILE   (sys:magic-const "QTFILE"))
(define sys:RFNAMEG  (sys:magic-const "RFNAMEG"))
(define sys:RFENVG   (sys:magic-const "RFENVG"))
(define sys:RFFDG    (sys:magic-const "RFFDG"))
(define sys:RFNOTEG  (sys:magic-const "RFNOTEG"))
(define sys:RFPROC   (sys:magic-const "RFPROC"))
(define sys:RFMEM    (sys:magic-const "RFMEM"))
(define sys:RFNOWAIT (sys:magic-const "RFNOWAIT"))
(define sys:RFCNAMEG (sys:magic-const "RFCNAMEG"))
(define sys:RFCENVG  (sys:magic-const "RFCENVG"))
(define sys:RFCFDG   (sys:magic-const "RFCFDG"))
(define sys:RFREND   (sys:magic-const "RFREND"))
(define sys:RFNOMNT  (sys:magic-const "RFNOMNT"))
(define sys:PNPROC   (sys:magic-const "PNPROC"))
(define sys:PNGROUP  (sys:magic-const "PNGROUP"))

(define (make-dir-struct d)
  (vector-set! d 0 *dir-type-tag*)
  d)

(define sys:dirstat
  (let ((make-dir-struct make-dir-struct))
    (lambda (s)
      (make-dir-struct (sys:convM2D (sys:stat s))))))

(define sys:dirfstat
  (let ((make-dir-struct make-dir-struct))
    (lambda (i)
      (sys:convM2D (sys:fstat i)))))

(define (sys:dirwstat s st)
  (dir:wstat s (sys:convD2M st)))

(define (sys:dirfwstat i st)
  (dir:fwstat s (sys:convD2M st)))

(define (is-dir? file)
  (let ((qid (dir-qid (sys:dirstat file))))
    (= #x80 (bit* (string->number (substring qid 26 28) 16) #x80))))

(define (sys:getenv name)
  (let ((fd (sys:open (string-append "/env/" name) sys:OREAD)))
    (if fd
        (let* ((len (sys:seek fd 0 2))
               (val (sys:pread fd len 0)))
          (sys:close fd)
          val)
        #f)))

(define (sys:putenv name val)
  (let ((fd (sys:create (string-append "/env/" name) sys:OWRITE #o666)))
    (if fd
        (let ((r (sys:write fd val)))
          (sys:close fd)
          r)
        #f)))

(define (sys:dirreadall d)
  (error "not implemented"))

(define (sys-plan9:sys-plan9) #t)
