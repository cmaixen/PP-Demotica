#lang racket

(#%require "Steward.rkt")
(require db)
(#%require "Logsystem.rkt")
(#%require "Constants.rkt")
(#%require "Majordomo.rkt")
(#%require "Utillities.rkt")


(send (make-majordomo) 'start)   
