#lang racket

(require racket/generator)
(require data/heap)
(require "exceptions.rkt")
(require "events.rkt")

(provide environment<%>
         environment%)

(struct item
        (timestamp priority id event)
        #:transparent)

(define-syntax-rule (<>-or x y body)
  (let ([rx x]
        [ry y])
    (if (< rx ry)
      #t
      (if (> rx ry)
        #f
        body))))

(define (event<=? e1 e2)
  (<>-or (item-timestamp e1) (item-timestamp e2)
         (<>-or (item-priority e1) (item-priority e2)
                (<>-or (item-id e1) (item-id e2)
                       #t))))

(define environment<%>
  (interface () now active-process schedule! step! run!))

(define environment%
  (class* object% (environment<%>)
          (init [initial-time 0])

          (define _now initial-time)
          (define _queue (make-heap event<=?))
          (define _active-process #f)

          (super-new)
          
          (define next-id (sequence->generator (in-naturals 0)))

          (define/public (now) _now)

          (define/public (active-process) _active-process)

          (define/public (set-active-process! active-proc)
            (set! _active-process active-proc))

          (define/public (schedule! event)
            (heap-add! _queue
                       (item
                         (+ _now (send event delay))
                         (send event priority)
                         (next-id)
                         event)))

          (define/public (step!)
            (when (<= (heap-count _queue) 0)
              (raise-empty-schedule))
            (match-let ([(item timestamp _ _ event) (heap-min _queue)])
              (heap-remove-min! _queue)
              (set! _now timestamp)
              ; call callbacks in reverse order because we appended them with cons
              (for ([callback (reverse (send event callbacks))])
                  (callback event))
              (send event set-processed!)
              (when (not (send event ok?))
                (raise (send event value)))))

          (define/public (run! [until #f])
            (when until
              (when (<= until _now)
                (raise-arguments-error 'run "<= until _now" until))
              (let ([ev (new event%
                             [init-env this]
                             [init-delay (- until _now)]
                             [init-priority priority-urgent])])
                (send ev set-ok! #t)
                (send ev add-callback!
                      (lambda (event) (raise-empty-schedule)))
                (schedule! ev)))
            (with-handlers ([exn:fail:simrkt:empty-schedule?
                             (lambda (e) (void))])
              (step-repeatedly!)))

          (define/private (step-repeatedly!)
            (step!)
            (step-repeatedly!))

          (define/public (make-process generator)
            (new process% [init-env this] [init-generator generator]))

          (define/public (make-timeout delay)
            (new timeout% [init-env this] [init-delay delay]))
          ))

