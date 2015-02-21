#lang racket

(require racket/generator)
(require "exceptions.rkt")

(provide priority-urgent
         priority-normal
         event%
         timeout%
         process%)

(define priority-urgent  0)
(define priority-normal  1)

(define event%
  (class object%
         (init init-env [init-delay 0] [init-priority priority-normal])
         (super-new)

         (define _env init-env)
         (define _delay init-delay)
         (define _priority init-priority)
         (define _callbacks empty)
         (define _value null)
         (define _ok #f)

         (define/public (env) _env)

         (define/public (delay) _delay)

         (define/public (priority) _priority)

         (define/public (callbacks) _callbacks)

         (define/public (add-callback! callback)
           (when (false? _callbacks)
             (raise-already-processed))
           (set! _callbacks (cons callback _callbacks)))

         (define/public (value) _value)

         (define/public (set-value! val)
           (set! _value val))

         (define/public (ok?) _ok)

         (define/public (set-ok! ok)
           (set! _ok ok))

         (define/public (processed?)
           (false? _callbacks))

         (define/public (set-processed!)
           (set! _callbacks #f))

         (define/public (succeed! [val null])
           (set! _ok #t)
           (set! _value val)) 

         (define/public (fail! [val null])
           (set! _ok #f)
           (set! _value val))
         ))

(define timeout%
  (class event%
         (init init-env init-delay)
         (super-new [init-env init-env] [init-delay init-delay])

         (inherit set-ok!)
         (set-ok! #t)
         ))

(define process%
  (class event%
         (init init-env init-generator)
         (super-new [init-env init-env])

         (define _generator init-generator)
         (define _target null)

         (inherit env set-ok!)

         ; schedule first call to resume
         (let ([ev (new event% [init-env init-env] [init-priority priority-urgent])])
           (send ev add-callback! (lambda (e) (send this resume e)))
           (send ev set-ok! #t)
           (send init-env schedule! ev)
           (set! _target ev))

         (define/public (target) _target)

         (define/public (resume wakeup-event)
           (send (env) set-active-process! this)
           (let ([ev (_generator this wakeup-event)])
             (if (eq? (generator-state _generator) 'done)
               ; _generator has already yielded the last event.
               (begin
                 (set-ok! #t)
                 (send (env) schedule! this))
               ; _generator returned another event to wait upon.
               (begin
                 (send ev add-callback! (lambda (e) (send this resume e)))
                 (send (env) schedule! ev)
                 (set! _target ev))))
           (send (env) set-active-process! #f))
         ))

