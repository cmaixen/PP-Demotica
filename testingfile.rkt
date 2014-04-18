#lang racket
(require rackunit)
(#%require "Steward.rkt")
(require db)
(#%require "Logsystem.rkt")
(#%require "Constants.rkt")
(#%require "Majordomo.rkt")
(#%require "Utillities.rkt")

;UNIT DEVICES
;-----------
;Test

(display ">> Testing Up and Running...")
(newline)


;testen van een object
(define name "test-object")
(define serial "XXXXX")
(define com-adres "XXXXX")
(define type temperaturesensor_type)
(define location "Bathroom")


(define (find-in-list string lst)
  (define (loop lst)
    (if (empty? (cdr lst))
        (equal? (car lst) string)
        (if (equal? (car lst) string)
            #t
            (loop (cdr lst)))))
  (loop lst))


(test-case
 "System Testing"
 (display ">> Initializing Majordomo...")
 (newline)
 
 (define test-majordomo (make-majordomo))
 
 (display ">> Majordomo Up and Running...")
 (newline)
 
 (display ">> Adding device...")
 (newline)
 (send (send test-majordomo 'get-steward location) 'add-device type name serial com-adres)
 
 ;checks of added data is the same
 
 (check-not-false (find-in-list name (send (send test-majordomo 'get-steward location) 'list_devices_names)))
 (check-not-false (find-in-list type (send (send test-majordomo 'get-steward location) 'list_devices_type)))
 (check-not-false (find-in-list location (send (send test-majordomo 'get-steward location) 'list_devices_location)))
 (check-not-false (find-in-list com-adres (send (send test-majordomo 'get-steward location) 'list_devices_com_adress)))
 (check-not-false (find-in-list serial (send (send test-majordomo 'get-steward location) 'list_devices_serial)))
 
 (display ">> Delete Device...")
 (newline)
 
 (send (send test-majordomo 'get-steward location) 'delete_device name)
 
 (display ">> Testing was Succesfull!")
 (newline)
 
 ;exit for ending the threads of the majordomo en exit process
 (exit)
 )

