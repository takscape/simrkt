#lang racket

(provide exn:fail:simrkt:empty-schedule?
         exn:fail:simrkt:already-processed?
         raise-empty-schedule
         raise-already-processed)

(struct exn:fail:simrkt exn:fail ()
        #:transparent)

(struct exn:fail:simrkt:empty-schedule exn:fail:simrkt ()
        #:transparent)

(struct exn:fail:simrkt:already-processed exn:fail:simrkt ()
        #:transparent)

(define (raise-empty-schedule)
  (raise (exn:fail:simrkt:empty-schedule
           "Empty schedule."
           (current-continuation-marks))))

(define (raise-already-processed)
  (raise (exn:fail:simrkt:already-processed
           "Event has already been processed."
           (current-continuation-marks))))

