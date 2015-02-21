#lang racket

(require racket/generator)
(require "core.rkt")

(module+ main
  (define env (new environment%))
  (define fooproc
    (generator (proc ev)
               (printf "Process 1, now=~a~n" (send env now))
               (yield (send env make-timeout 10))
               (printf "Process 2, now=~a~n" (send env now))
               (yield (send env make-timeout 20))
               (void)))
  (define proc (send env make-process fooproc))
  (send env run!)
  (printf "Current time=~a~n" (send env now))
  (flush-output))

