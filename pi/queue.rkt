#lang R5RS

(#%provide make-queue)
(#%require racket/base)
(#%require "Utillities.rkt")

(define (make-queue)
 
  (let ((queue_info '()))
    
    
   (define  (enqueue! value)
     (set! queue_info (append queue_info (list value))))
    
    
    
    (define (dequeue!)
      (if (null? queue_info)
          (display "ERROR queue is empty!")
         (let ((val (car queue_info)))
      (set! queue_info (cdr queue_info))
           val)))
      
    
    (define (queue-empty?)
      (null? queue_info))
    
    
    (define (empty-queue!)
      (set! queue_info '()))
    
    (define (print-queue)
      (display queue_info))
    
(define (dispatch message)
  (case message
    ((enqueue!) enqueue!)
    ((dequeue!) dequeue!)
    ((empty-queue!) empty-queue!)
    ((queue-empty?) queue-empty?)
    ((print-queue) print-queue)
    (else (display "ERROR QUEUE unknown message"))))
    
    dispatch))

