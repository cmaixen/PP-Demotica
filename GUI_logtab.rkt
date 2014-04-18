;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Domotica Energie Monitoring Systeem                  *-*-
;-*-*                        Yannick Merckx                           *-*-
;-*-* Programmeerproject 2013-2014 2de Bachelor Computerwetenschappen *-*-
;-*-*           Student from Vrije Universiteit Brussel               *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


; Beschrijving: ADT Logtab ; Argumenten: GUI-object
; Output: Logtab object
; Messages:

; - get-logtab: argumenten: geen 
;              output: panel%
; - update-log: argumenten: geen
;                output:  void

; Commentaar: geen


#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(provide make_GUI_logtab)
(#%require "Constants.rkt")


(define (make_GUI_logtab GUI)
  (let ((tab-panel (util:send GUI 'get-tabpanel)))
  
    
    
    ;accessor
    ;--------
    
    ;geeft logtab terug
    (define (get-logtab)
      logtab)
    
    
    
    ;aanmaken van verticaal paneel
    (define logtab (new vertical-panel% 
                      [parent tab-panel]
                      [alignment (list 'center 'center)]
                      
                      ))
    
    
    ;aanmaken van tekst object
    (define t (new text% ))

    ;aanmaken van editor-panel 
    (define log-panel (new editor-canvas%	 
                           [parent logtab]
                           (editor t)))
  
    
    ;update van de log door gewoon de file terug in te laden
    (define (update-log) 
     (send t load-file  logfile)
        )
  
    ;logfile inloaden in textobject dat gebonden is aan editorcanvas
    
    (define (dispatch message)
      (case message
        ((get-logtab) get-logtab)
        ((update-log) update-log)
        (else (error 'logtab "unknown message ~a" message))))
    dispatch ))
    
    