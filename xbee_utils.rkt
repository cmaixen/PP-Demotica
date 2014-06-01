#lang r5rs


(#%require rnrs/bytevectors-6)


(#%provide (all-defined))

;Constants ZIGBEE PROTOCOL
;-------------------------


(define Transmit_request 16)
(define Transmit_status 139)
(define Transmit_answer 144)

(define message-acknowledged 1)
(define transmit_retry_counter 0)
(define delivery_status_succes 0)
(define discovery_status_succes 1)