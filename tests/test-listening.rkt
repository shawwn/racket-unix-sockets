(require "../socket.rkt")

(define (serve path)
  (define listener (unix-socket-listen path 5))
  (printf "listening: ~a" listener)
  (newline)
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(define (accept-and-handle listener)
  (define-values (i o) (unix-socket-accept listener))
  (printf "accepted: ~a" listener)
  (newline)
  (display (read-line i))
  (newline))

(serve "tmp-socket")

