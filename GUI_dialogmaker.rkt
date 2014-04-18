;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Domotica Energie Monitoring Systeem                  *-*-
;-*-*                        Yannick Merckx                           *-*-
;-*-* Programmeerproject 2013-2014 2de Bachelor Computerwetenschappen *-*-
;-*-*           Student van Vrije Universiteit Brussel                *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Beschrijving: ADT Dialog ; Argumenten: ADT GUI, naam van dialoog (string?)
; Output: Dialog object
; Messages:

; - show-dialog: argumenten: geen
;          output: void

; - get-dialog: argumenten: geen
;                 output: dialog%

; - hide-dialogx: argumenten: geen
;                        output: void

; Commentaar: Objecten voor in de dialoog worden na het aanmaken toegevoegd

#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(#%require  "Constants.rkt")

(provide make-dialog)



(define (make-dialog GUI titel)
  
     ;dialoog   
    (define dialog (new dialog% [parent (util:send GUI 'get-mainframe)] 
                            [label titel]))
    
    
    ;verticaal paneel voor dialoog
    (define dialog_panel (new vertical-panel% [parent dialog]
                                  [min-width min_width_dialog]
                                  [alignment '(center center)]
                                  [min-height min_height_dialog]))    
  
   
  (define (show-dialog)
    (send dialog show #t))
   
  
  (define (hide-dialog)
    (send dialog show #f))
  
  (define (get-dialog)
    dialog_panel)
  
 
  
  

        
                
         (define (dispatch message)
           (case message 
             ((show-dialog) show-dialog)
             ((hide-dialog) hide-dialog)
             ((get-dialog) get-dialog)
             
             (else (error 'dialog "unknown message ~a" message))))
  dispatch)