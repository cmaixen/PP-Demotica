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

; Beschrijving: ADT Loginscreen ; Argumenten: Majordomoobject , GUI-object, logsystem-object
; Output: Loginscreen object
; Messages:

; - get-loginscreen: argumenten: geen
;                    output: Dialog%

; Commentaar: geen

#lang racket
(require racket/gui/base)
(require db)
(#%require (prefix util: "Utillities.rkt"))
(#%require "Constants.rkt")
(provide create-loginscreen)


(define (create-loginscreen majordomo GUI logsystem)
  (let ((db (util:send majordomo 'get-db))
        (mainframe (util:send GUI 'get-mainframe)))
        
  
  (define (get-loginscreen)
    dialog)
  
    ; LOGIN
    ;-------
    
    ;Username check
    (define (user_check name)
      (query-maybe-value db "select Username from user_system where Username = $1"  name))
    
    (define (get-password  name)
      (query-value db "select Password from user_system where Username = $1"  name))
    
    ;test of username aanwezig is en of passwoord dan correct is.
    ;lazy and is hier ideaal vanwege het feit dat het eerst gaat controleren of de username bestaat
    
    (define (check_login name password)
      (let ((result (and (user_check name) (equal? password (get-password name)))))
        (if result
            (util:send logsystem 'succesfull-login name)
            (util:send logsystem 'failed-login name))
        result
      ))
    
 ; Create a dialog
    (define dialog (new dialog% [parent mainframe] 
                        [label "Dialog"]))
    
    
    
    
    (define dialog_panel (new vertical-panel% [parent dialog]
                              [min-width min_width_dialog]
                              [alignment '(center center)]
                              [min-height min_height_dialog]))
    
    (define Button_panel (new horizontal-panel% [parent dialog]
                              [alignment '(center center)]
                              [min-width min_width_button_panel] 
                              [min-height min_height_button_panel ]))
    
    ; Voeg boodschap toe aan dialoog 
    (define message (new message%	 
                         [label "Welcome"]	 
                         [parent dialog_panel]
                         [enabled #t]	   	 	 
                         [min-width min_width_dialog]	 
                         [min-height min_height_dialog]	 
                         [auto-resize #t]))
    
    
    
    ;Textfields loginscreen
    
    (define username_input (new text-field% [parent dialog_panel] [label "Username"]))
    (define password_input (new text-field% [parent dialog_panel] [style '(single password)] [label "Password "]))
    
    
    ;voeg Cancel- en Ok-button toe aan het horizontale paneel
    (define button-login (new button% [parent  Button_panel ] [label "Login"] [callback (lambda (button-cancel click)
                                                                                          (print (user_check (send (send username_input get-editor) get-text)))
                                                                                          (if (check_login (send (send username_input get-editor) get-text) (send (send password_input get-editor) get-text) )
                                                                                              (begin (send dialog show #f)
                                                                                                    
                                                                                                     (util:send majordomo 'set-current-user  (send (send username_input get-editor) get-text))
                                                                                                     (send mainframe set-label "Welcome to your personal Electricity Home Monitoring System"))
                                                                                              (send message set-label "Wrong Username/Password"))
                                                                                          
                                                                                          )]))
    
    (define button-cancel (new button% [parent  Button_panel ] [label "Cancel"] [callback
                                                                                 (lambda (button-cancel click)
                                                                                   (send dialog show #f)
                                                                                   (send mainframe on-exit)
                                                                                   
                                                                                   )]))
    
    
    
  (define (dispatch message)
    (case message
      ((get-loginscreen) get-loginscreen)
    (else (error 'GUI_loginscreen "unknown message ~a" message))))
  
  dispatch
  ))
   
    