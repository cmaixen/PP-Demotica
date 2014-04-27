#lang racket

(require db)
(#%require "Logsystem.rkt")
(#%require "Constants.rkt")
(#%require "Majordomo.rkt")
(#%require "Utillities.rkt")


;Before you running this file you need to run the Virtual Steward files, 3 in total.
;These files simulate the running stewards in the different rooms.

(send (make-majordomo) 'start)   
