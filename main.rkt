#lang racket/base
(require ffi/unsafe
         ffi/file
         (rename-in racket/contract [-> fn->]))
(provide/contract
  [unix-socket-connect (fn-> path-string?
                             (values input-port? output-port?))]
  [unix-socket-listen  (fn-> path-string? number?
                             number?)]
  [unix-socket-accept  (fn-> number?
                             (values input-port? output-port?))])

(define (cleanse-unix-socket-path fn path)
  (let ([p (path->bytes (cleanse-path (path->complete-path path)))])
    (unless (< (bytes-length p) 100)
      (error fn "expected path of less than 100 bytes, got ~s" p))
    p))

;; unix-socket-connect : path -> input-port output-port
;; Connects to the specified path.
(define (unix-socket-connect path)
  (security-guard-check-file 'unix-socket-connect path '(read write))
  (let* ([p (cleanse-unix-socket-path 'unix-socket-connect path)]
         [addr (make-unix-sockaddr p)]
         [addrlen (+ (ctype-sizeof _short) (bytes-length p))]
         [s (make-socket 'unix-socket-connect)])
    (unless (positive? s)
      (error 'unix-socket-connect "failed to create socket for path: ~s (errno=~a)" p (saved-errno)))
    (unless (zero? (_connect s addr addrlen))
      (_close s)
      (error 'unix-socket-connect "failed to connect socket to path: ~s (errno=~a)" p (saved-errno)))
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) (_close s) (raise e))])
      (_make_fd_output_port s 'socket #f #f #t))))

;; unix-socket-listen : path number -> number
;; Creates a unix domain socket.  "maxpending" is the max number of
;;  pending connections allowed.
(define (unix-socket-listen path maxpending)
  (security-guard-check-file 'unix-socket-listen path '(read write))
  (let* ([p (cleanse-unix-socket-path 'unix-socket-listen path)]
         [s (make-socket 'unix-socket-listen)]
         [addr (make-unix-sockaddr p)]
         [addrlen (+ (ctype-sizeof _short) (bytes-length p))])
    (unless (positive? s)
      (error 'unix-socket-listen "failed to create socket: ~s (errno=~a)" p (saved-errno)))
    (unless (zero? (_bind s addr addrlen))
      (_close s)
      (error 'unix-socket-listen "failed to bind socket to path: ~s (errno=~a)" p (saved-errno)))
    (unless (zero? (_listen s maxpending))
      (_close s)
      (error 'unix-socket-listen "failed to listen on path: ~s (errno=~a)" p (saved-errno)))
    s))

;; unix-socket-accept : number -> input-port output-port
;; Accepts a connection from the unix domain socket.
(define (unix-socket-accept s)
  (let-values ([(them addr) (_accept s (sockaddr-len))])
    (unless (> them -1)
      (error 'unix-socket-accept "failed to accept via socket ~a (errno=~a)" s (saved-errno)))
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) (_close them) (raise e))])
      (_make_fd_output_port them 'socket #f #f #t))))
  

(define platform
  (let ([os (system-type 'os)]
        [machine (system-type 'machine)])
    (cond [(eq? os 'macosx) 'macosx]
          [(regexp-match #rx"Linux.*86" machine) 'linux86]
          [(regexp-match #rx"SunOS" machine) #f #;'solaris]
          [else #f])))

(define _socklen_t _uint)
(define _size_t _int)

(define AF_UNIX 1)
(define SOCK_STREAM
  (case platform
    ((linux86 macosx) 1)
    ((solaris) 2)
    (else #f)))

(define (make-socket fn)
  (unless (and AF_UNIX SOCK_STREAM)
    (raise-user-error fn "unix-domain sockets not supported on this platform"))
  (_socket AF_UNIX SOCK_STREAM 0))

(define _sockaddr_un_path_part
  (case platform
    ((linux86 solaris)
     (make-cstruct-type (build-list 108 (lambda (i) _byte))))
    ((macosx)
     (make-cstruct-type (build-list 104 (lambda (i) _byte))))
    (else
     ;; kluge: so that later definitions go through.
     _int)))

(define-cstruct _sockaddr_un
  ([sun_family _short]
   [sun_path   _sockaddr_un_path_part]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   _sockaddr_un_path_part]))

(define (ffi name type)
  (case platform
    ((linux86 solaris macosx)
     (get-ffi-obj name #f type (lambda () #f)))
    (else
     (lambda _ (error name "not supported")))))

(define _socket (ffi "socket"
  (_fun #:save-errno 'posix _int _int _int -> _int)))

(define _connect (ffi "connect"
  (case platform
    ((linux86 solaris)
     (_fun #:save-errno 'posix _int _sockaddr_un-pointer _int -> _int))
    ((macosx)
     (_fun #:save-errno 'posix _int _macosx_sockaddr_un-pointer _int -> _int)))))

(define _bind (ffi "bind"
  (case platform
    ((linux86 solaris)
     (_fun #:save-errno 'posix _int _sockaddr_un-pointer _int -> _int))
    ((macosx)
     (_fun #:save-errno 'posix _int _macosx_sockaddr_un-pointer _int -> _int)))))

(define _listen (ffi "listen"
  (_fun #:save-errno 'posix _int _int -> _int)))

(define _accept (ffi "accept"
  (case platform
    ((linux86 solaris)
     (_fun #:save-errno 'posix _int (addr : (_ptr o _sockaddr_un)) (len : (_ptr i _socklen_t)) -> (socket : _int) -> (values socket addr)))
    ((macosx)
     (_fun #:save-errno 'posix _int (addr : (_ptr o _macosx_sockaddr_un)) (len : (_ptr i _socklen_t)) -> (socket : _int) -> (values socket addr))))))

(define _setsockopt (ffi "setsockopt"
  (_fun _int _int _int _pointer _socklen_t -> _int)))

(define _close (ffi "close"
  (_fun _int -> _int)))

(define _make_fd_output_port (ffi "scheme_make_fd_output_port"
  (_fun _int _scheme _bool _bool _bool -> _scheme)))


(define (make-unix-sockaddr path)
  (case platform
    ((linux86 solaris)
     (make-sockaddr_un AF_UNIX path))
    ((macosx)
     (make-macosx_sockaddr_un (bytes-length path) AF_UNIX path))))

(define (sockaddr-len)
  (case platform
    ((linux86 solaris)
     (ctype-sizeof _sockaddr_un))
    ((macosx)
     (ctype-sizeof _macosx_sockaddr_un))))
