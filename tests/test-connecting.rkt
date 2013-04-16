(require "../main.rkt")

(define-values (i o) (unix-socket-connect "tmp-socket"))
(display "hello" o)

