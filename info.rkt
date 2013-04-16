#lang setup/infotab

(define name "racket-unix-sockets")
(define compile-omit-paths
  '("tests"))
(define blurb
  '("Provides unix domain sockets."))
(define categories '(io net))
(define can-be-loaded-with 'all)
(define primary-file "main.rkt")
(define required-core-version "5.1")
(define repositories '("4.x"))
