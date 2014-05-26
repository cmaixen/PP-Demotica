#lang R5RS

(#%require "Utillities.rkt")
(#%provide (all-defined))

;De Lokale xbee bepalen
 (define XBEE_name #"/dev/ttyUSB0")
 ;xbee object initialiseren
 (define xbee (xbee-initialise XBEE_name 9600))


;initialiseren van het xbee-object
 (define (xbee-initialise XBEE baudrate)
;het maken van een xbee object
  (make-xbee))

;weergeven van al de devices
 (define (xbee-list-nodes xbee)
  (send xbee xbee-list-nodes))
    
    
    (define (xbee-discover-nodes xbee)
      (send xbee 'xbee-discover-nodes))
    
    
   ;inlezen van xbee buffer 
  (define (xbee-tick xbee)
  (set! steward_buffer (send xbee 'xbee-tick)))
    
    
    ;message is al geconverteerd naar vector
    ;converteren gebeurd op de steward
    (define (xbee-write xbee device-adress message)
      (send xbee 'xbee_write device-adress message))