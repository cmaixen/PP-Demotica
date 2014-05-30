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


; Beschrijving: ADT GUI ; Argumenten: Majordomo-object, logsystem-object
; Output: gui object
; Messages:
; - start: argumenten: geen
;          output: void

; - get-tabpanel: argumenten: geen
;                 output: tab-panel%

; - get-current-listbox: argumenten: geen
;                        output: list-box%

; - set-current-listbox: argumenten: list-box% (listbox dat we als current-listbox willen zetten)
;                        output: void
;
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(require db)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_tab1-1.rkt")
(#%require "GUI_loginscreen.rkt")
(#%require "GUI_logtab.rkt")
(#%require "GUI_analysticstab.rkt")
(#%require "Constants.rkt")
(provide make-gui)

(define (make-gui majordomo logsystem) 
  (let* ((current-listbox 'NULL))      
      
    
    ;starten van de GUI
    (define (start)
      (send logscreen show #t))
   
    ;ACCESSOREN
    ;----------
    
    
    ;geeft het tab-panel terug
    (define (get-tabpanel) tab-panel)
    
    ;geeft de huidige listbox waar je in aan het werken bent terug
    (define (get-current-listbox)
      current-listbox)
    
    ;geeft het hoofdframe terug
    (define (get-mainframe)
      mainframe)

    ;MUTATOREN
    ;---------
    
    ;de huidige listbox veranderen in de gegeven
    (define (set-current-listbox new-listbox)
      (set! current-listbox new-listbox))
    

    ;functie voor dispatchingsysteem
    (define (dispatch message)
      (case message 
        ((start) start)
        ((get-tabpanel) get-tabpanel)
        ((get-current-listbox) get-current-listbox)
        ((set-current-listbox) set-current-listbox)
        ((get-mainframe) get-mainframe)
        (else (error 'GUI "unknown message ~a" message))))
    
    
    

    ;GUI initializeren
    ;-----------------
       ;Definieren van het hoofdframe
    
    (define mainframe (new frame% [label "Log in"]
                           [width mainframe_width]
                           [height mainframe_height]))
    
    ;tabpanel
    
    (define tab-panel
      (new tab-panel%	 
           [choices (list "Rooms" "Graphs" "Logs")]	 
           [parent mainframe]
           [callback (lambda (tab-panel click)
                       (case (send tab-panel get-selection)
                         ((0) (send tab-panel change-children (lambda (x) (list tab1))))
                         ((1) (send tab-panel change-children (lambda (x) (list analysticstab))))
                         (else 	 (util:send logtab-object 'update-log)	 	 	
                                    (send tab-panel change-children (lambda (x) (list logtab))) )
                         ))]))
    
    
    ;Loginscreen
    ;----------
    (define  loginscreen-object (create-loginscreen majordomo dispatch logsystem))
    
    (define logscreen
      (util:send loginscreen-object  'get-loginscreen))
    
    ;Tab1
    ;----
    
    (define tab1-object (make_GUI_tab1-1 majordomo dispatch))
    (define tab1 (util:send tab1-object 'get-tab1))
    
    
    
    ;TAB2
    ;----
    
    (define analysticstab-object (make_GUI_analysticstab dispatch))
    (define analysticstab (util:send analysticstab-object 'get-analysticstab))
    
    
    
    ;TAB3
    ;----
    
    (define logtab-object (make_GUI_logtab dispatch))
    (define logtab (util:send logtab-object 'get-logtab))
    
    
    ;initialization tabpanel
    (send tab-panel change-children (lambda (x) (list tab1)))
   
     ;TAB 5
    ;-------
    ;userinfo
    
    
    dispatch
    ))



