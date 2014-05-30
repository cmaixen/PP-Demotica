#lang racket
(require racket/gui/base)
(require racket/draw)


(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(provide make_GUI_tab1-3-powerplug)


(define (make_GUI_tab1-3-powerplug majordomo GUI name_of_device logsystem)
  (let ((tab-panel (util:send GUI 'get-tabpanel))
        (mainframe (util:send GUI 'get-mainframe))
        (slider-enabled #f)
        (is_pow_on 'null))
    
    
    (define (is_pow_on?)
      (let* ( (GET_answer (util:send majordomo 'request "GET\n" name_of_device))
              (lst (split-string-in-list GET_answer #\newline))
              (pow_statement (car lst)))
        (if (equal? pow_statement "POW=on")
            (set! is_pow_on #t)
            (set! is_pow_on #f))
        is_pow_on))
    
    
    
    
    
    
    ;ACCESSOREN
    ;----------
    
    (define (get-tab)
      tab1-3)
    
    
    ;hoofdcontainer van de tab
    (define tab1-3 (new vertical-panel%
                        [parent tab-panel]
                        [alignment (list 'center 'center)]))
    
    
    ;opbouw van de tab
    
    ;---------------------------------------
    ;                |                     |
    ;                |  device_info_panel  |
    ;   image_panel  |---------------------|
    ;                |  change_panel       |
    ;                |                     |
    ;--------------------------------------|
    ;                                      |
    ;           editor_panel               |
    ;                                      |
    ;---------------------------------------
    
    
    ;container bovenste helft
    (define tab_info (new horizontal-panel%
                          [parent tab1-3]	 
                          [alignment (list 'center 'top)]
                          ))
    
    ;editor voor de onderste helft van het scherm
    (define editor-canvas   (new editor-canvas%	 
                                 [parent tab1-3]
                                 ))
    
    
    ;linkerbovenhoek van het canvas 
    (define image_tab (new vertical-panel%
                           [parent tab_info]
                           [spacing 0]	 
                           [style (list 'border)]
                           [vert-margin 0]
                           [alignment (list 'center 'top)]
                           ))
    
    
    ;Afbeelding voor in linkerbovenhoek    
    (define l (new message% [parent image_tab] 
                   [label (make-object bitmap% "powerplug.png" 'png/alpha)]
                   [stretchable-width #t]	 
                   [stretchable-height #t]	 
                   [auto-resize #t]
                   )) 
    
    
    ;rechterbovenhoek van het canvas
    (define device_info_canvas (new vertical-panel%
                                    [parent tab_info]
                                    [spacing 0]                           
                                    [alignment (list 'left 'top)]
                                    ))
 
    ;bovenste helft van linkerbovenhoek
    (define device_info_canvas_info(new horizontal-panel%
                                         [parent device_info_canvas]
                                         [spacing 0]
                                         [style (list 'border)]
                                         
                                         [alignment (list 'left 'center)]
                                         ))
    
        ;bovenste helft van linkerbovenhoek
    (define device_info_canvas_info_GET (new vertical-panel%
                                         [parent device_info_canvas_info]
                                         [spacing 0]
                                         [style (list 'border)]
                                         
                                         [alignment (list 'left 'top)]
                                         ))
   
    ;bovenste helft van linkerbovenhoek
    (define device_info_canvas_info_DEV(new vertical-panel%
                                         [parent device_info_canvas_info]
                                         [spacing 0]
                                         [style (list 'border)]
                                         
                                         [alignment (list 'left 'top)]
                                         ))
    
    ;afbeelden van de info in het infovenster
    (define (create_info_panel  panel list_of_strings)
      (define (loop lst)
        (  if (empty? lst)
              'done
              (let ((message (car lst)))
                (new message% [parent panel] 
                     [label message]
                     )
                (loop (cdr lst)))))
      (loop list_of_strings))
    
    
    
    ;onderhleft van de rechterbovenhoek
    (define change_panel (new vertical-panel%
                              [parent device_info_canvas]
                              [alignment (list 'left 'center)]
                              [style (list 'border)]
                              ))
    
    ;geeft een status box weer die het toestel aan of uitzet
    (define statusbox (new check-box%	 
                           [label "Turn Device Off"]	 
                           [parent change_panel]
                           [vert-margin 20]
                           
                           [value #f]))
    
    ;paneel dat de slider en de choices bij elkaar houd
    (define setter_panel (new horizontal-panel%
                              [parent change_panel]
                              [spacing 0]
                              
                              [vert-margin 20]
                              [alignment (list 'left 'center)]
                              ))
    
    (define (get-unit choice)
     ( cond ((equal? choice "FREQ")
         "Hz")
         ((equal? choice "VRMS")
     "V")
         ((equal? choice "IRMS")
          "mA")
         ((equal? choice "LOAD")
          "W")
         (else "kWh")
         ))

    
    
    (define (compose_set_command choice slider)
      (let ((value (send slider get-value)))
      (format "SET ~a=~a~a\n" choice value (get-unit choice))))
    
     (define (compose_print_command choice slider)
      (let ((value (send slider get-value)))
      (format "SET ~a=~a" choice value)))
    
     (define (check_ack string)
     (eq? (string-ref string 0) #\a ))

    (define confirm_buttom (new button% [parent change_panel] 
                                [label "Confirm Changes"]
                                [vert-margin standaardmargin_button]	
                                [horiz-margin standaardmargin_button]	 
                                [min-width standaardmargin_button]	 
                                [min-height standaardmargin_button]
                                [callback (lambda (button click) (let* ((text (new text%))
                                                                        (device-name (util:send majordomo 'get-current-device))
                                                                        (current-steward (util:send majordomo 'get-current-steward))
                                                                        (turn_device_off  (send statusbox get-value))
                                                                        (is_pow_on (is_pow_on?)))
                                                                   
                                                                   
                                                                   ;toestel staat aan en user wenst deze uit te zetten
                                                                   (cond ((and turn_device_off is_pow_on)
                                                                          (let ((answer (util:send majordomo 'request "SET POW=OFF\n" name_of_device)))
                                                                            (send text insert (format ">> Shutting down  ~a in the ~a ... \n" device-name current-steward))
                                                                            (send text insert (format ">>> Answer: ~a \n" answer))
                                                                                (if (check_ack answer)
                                                                               ( util:send logsystem 'device-off current-steward name_of_device )
                                                                               'nack)))
                                                                          
                                                                         
                                                                         
                                                                         ;toestel staat uit en men wil het toestel uitzetten
                                                                         ((and turn_device_off (not is_pow_on))
                                                                          (send text insert (format ">> Power of device is already off \n")))
                                                                         
                                                                         ;device staat uit en men wil een commando sturen 
                                                                         ((not is_pow_on)
                                                                           (let ((answer  (util:send majordomo 'request "SET POW=ON\n"name_of_device)))
                                                                          ;zet het toestel aan
                                                                          (send text insert (format ">> Turn On ~a in the ~a ... \n" device-name current-steward))
                                                                          (send text insert (format ">>>>Answer: ~a \n " answer))
                                                                           (if (check_ack answer)
                                                                               ( util:send logsystem 'device-on current-steward name_of_device )
                                                                               'nack))
                                                                          
                                                                       (   if slider-enabled
                                                                          ;stuur commando 
                                                                          (let* ((commando (compose_set_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection))))
                                                                            (print_commando (compose_print_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection))))
                                                                            (answer (util:send majordomo 'request commando name_of_device)))
                                                                            
                                                                            
                                                                            (send text insert (format ">> Sending ~a to ~a in the ~a ... \n " print_commando device-name current-steward))
                                                 
                                                                            (send text insert (format ">>>> Answer: ~a" answer ))
                                                                            (if (check_ack answer)
                                                                               ( util:send logsystem 'change-mesurement current-steward name_of_device (send choice get-string-selection) (send (get-right-slider (send choice get-string-selection)) get-value))
                                                                               'nack))
                                                                          'done))
                                                                         ;stuur commando device staat al aan
                                                                         (else
                                                                          
                                                                          (if slider-enabled
                                                                          
                                                                          (let* ((commando (compose_set_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection))))
                                                                            (print_commando (compose_print_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection))))
                                                                             (answer (util:send majordomo 'request commando name_of_device)))
                                                                               (send text insert (format ">> Sending ~a to ~a in the ~a ... \n " print_commando device-name current-steward))
                                                                            (send text insert (format ">>>> Answer: ~a \n" answer))
                                                                               (if (check_ack answer)
                                                                               ( util:send logsystem 'change-mesurement current-steward name_of_device (send choice get-string-selection) (send (get-right-slider (send choice get-string-selection)) get-value))
                                                                            
                                                                               'nack))
                                                                          'done)))
                                                                         ;stuur commando 
                                                                   
                                                                   (send editor-canvas set-editor text)
                                                                   ;update info device
                                                                   (update_device_info)
                                                                   ))]))
    
    
    
    
    
    
    



;stelt een choicebox op met daarin al de keuzes

(define choice
  (new choice%     (label "Choice")
       (parent setter_panel)
       (choices (list "FREQ" "VRMS" "IRMS" "LOAD" "WORK"))
       [callback (lambda (c e) (display (send c get-string-selection)) 
                   (send setter_panel change-children (lambda (x) (list choice (get-right-slider (send c get-string-selection))))))]))



;return the right slider
(define (get-right-slider choice)
  (set! slider-enabled #t) 
  ( cond ((equal? choice "FREQ")
          FREQ-slider)
         ((equal? choice "VRMS")
          VRMS-slider)
         ((equal? choice "IRMS")
          IRMS-slider)
         ((equal? choice "LOAD")
          LOAD-slider)
         (else WORK-slider)
         ))



;verschillende sliders
;---------------------

(define FREQ-slider (new slider%
                         (label "Hz")
                         (parent setter_panel)
                         (min-value 0)
                         (max-value 100)
                         (init-value 42)))

(define VRMS-slider (new slider%
                         (label "V")
                         (parent setter_panel)
                         (min-value 0)
                         (max-value 100)
                         (init-value 42)))
(define IRMS-slider (new slider%
                         (label "mA")
                         (parent setter_panel)
                         (min-value 0)
                         (max-value 100)
                         (init-value 42)))
(define LOAD-slider (new slider%
                         (label "W")
                         (parent setter_panel)
                         (min-value 0)
                         (max-value 100)
                         (init-value 42)))

(define WORK-slider (new slider%
                         (label "kWh")
                         (parent setter_panel)
                         (min-value 0)
                         (max-value 1)
                         (init-value 0)))

;enkel choice box instellen, pas bij selectie komt de slider  
(send setter_panel change-children (lambda (x) (list choice)))
  
    (define (check-policies)
          (if (util:send majordomo 'check-authorized)
              'ok
              (begin
              (send device_info_canvas delete-child change_panel)
              (new message% [parent device_info_canvas] 
                   [label (make-object bitmap% "no-permission.png" 'png/alpha)]
                   [stretchable-width #t]	 
                   [stretchable-height #t]	 
                   [auto-resize #t]
                   )) ))
    
    
(define (update_device_info)
  (send device_info_canvas_info_GET change-children (lambda (x) '()))
  (create_info_panel device_info_canvas_info_GET (split-string-in-list (util:send majordomo 'request "GET\n" name_of_device) #\newline))
  (util:send logsystem 'status-update (util:send majordomo 'get-current-steward) name_of_device))


(create_info_panel device_info_canvas_info_GET (split-string-in-list (util:send majordomo 'request "GET\n" name_of_device) #\newline) )
    (create_info_panel device_info_canvas_info_DEV (split-string-in-list (util:send majordomo 'request "DEV\n" name_of_device) #\newline) )

 

(define (dispatch message)
  (case message 
    ((get-tab) get-tab)
    ((check-policies) check-policies)
    (else (error 'Multisensor "unknown message ~a" message))))
dispatch
))











