#lang r5rs

(#%require "Utillities.rkt")
(#%require "Powerplug.rkt")
(#%require "energysensor.rkt")
(#%require "queue.rkt")
(#%require "xbee_utils.rkt")
(#%provide make-xbee)
(#%require racket/base)

(define (make-xbee)
  (let* ((buffer (make-queue))
         (discoverd #f)
         (devicelist '())
         (xbee-with-list-nodes '())
         (empty-buffer #f))
    
    
    (define (empty-xbee-buffer)
      (if empty-buffer
          (begin (send buffer 'empty-queue!)
                 (set! empty-buffer #f))
          'done))
    
    
    ;aanmaken van devices bepaald aantal devices en deze teruggeven 
    ;geeft niets terug
    (define (xbee-discover-nodes)
      (let ((device_one (make-powerplug))
            (device_two (make-energysensor)))
        ;lijst met xbee-device objecten
        (set! devicelist (list device_one device_two))
        ;al meteen xbee-list-nodes opbouwen
        ;gemakkelijker voor later
        (set! xbee-with-list-nodes (list (list (send device_one 'get_name) (send device_one 'get_64-adress))
                                      (list (send device_two 'get_name) (send device_two 'get_64-adress))))
        
        ;discovered op true zetten
        (set! discoverd #t)
        ))
    
    (define (give-xbee-list-nodes)
      xbee-with-list-nodes)
    
    (define (xbee-write device_adress message)
      (let ((device (get-object device_adress)))
        ;stuurt naar device en voegt frame toe aan de buffer 
        (newline)
        (newline) 
        (display "message: ")
        (display message)
        (newline)
        (process_answer (send device 'request message))))
    
    
    ;message is hier een string en maakt het finale answerframe
    (define (process_answer answer)
      (let ((receive_frame (cdr answer))
            (status_frame (car answer)))
        (newline)
        (newline)
        (newline)
        (display "receive_frame :")
        (display receive_frame)
        (display "status_frame :" )
        (display status_frame)
        (newline)
        (newline)
        (newline)
        (send buffer 'enqueue! status_frame)
        (send buffer 'enqueue! receive_frame)
        (display (send buffer 'queue-empty?)
                 )))
    
    
    
    ;geeft het device-object terug dat moet worden aangesproken
    (define (get-object given_device_adress)
      (newline)
      (display "given adress to xbee")
      (display given_device_adress)
      (newline)
     (display  xbee-with-list-nodes)
      
      (define (loop lst)
        (if (null? lst)
            (display  "Device not found")
            (let* ((first_device (car lst))
                   (device_adress (send first_device 'get_64-adress)))
              
              (display "first device:")
              (display first_device)
              (newline)
              (display "device_adress:")
              (display device_adress)
                (display given_device_adress)
              (display  (equal? device_adress given_device_adress))
              (if (equal? device_adress given_device_adress)
                  first_device
                  (loop (cdr lst))))))
      (loop devicelist))
    
    
    
    ;de queue leegmaken
    (define (empty-queue! queue)
      (define (loop queue)
        (if (send queue 'queue-empty?)
            'done
            (begin (send queue 'dequeue!)
                   (loop queue))))
      (loop queue))
    
    
    ;geeft de queu terug en maakt deze vervolgens leeg
    (define (xbee-tick)
      (let* ((xbee-buffer buffer))
        ;buffer van de xbee bij de volgende aanspreking leegmaken    
        (set! empty-buffer #t) 
        xbee-buffer))
    
    (define (dispatch message)
      ;kijken of de buffer leeggemaakt moet worden
      (empty-xbee-buffer)
      (case message
        ((xbee-write) xbee-write)
        ((xbee-tick) xbee-tick)
        ((xbee-list-nodes) give-xbee-list-nodes)
        ((xbee-discover-nodes) xbee-discover-nodes)
        (else (display "ERROR: XBEE unknown message"))))
    
    dispatch
    )
  )

;(define xbee (make-xbee))
;(send xbee 'xbee-discover-nodes)
;(send xbee 'xbee-write  #(0 19 162 0 64 148 36 184) #(71 69 84 10))
;(define stewardbuffer (send xbee 'xbee-tick))
;(newline)
;(newline)
;(newline)
;(display "Status Frame: ")
;(display (dequeue! stewardbuffer)) 
;(newline)
;(newline)
;(display "Receive Frame: ")
;(display (dequeue! stewardbuffer)) 




