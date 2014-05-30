#lang racket
(require racket/gui/base)
(require racket/draw)
(require "GUI_utillities.rkt")
(require "Constants.rkt")
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(provide make_GUI_tab1-3-multisensor)


(define (make_GUI_tab1-3-multisensor majordomo GUI name_of_device logsystem)
  (let ((tab-panel (util:send GUI 'get-tabpanel))
        (mainframe (util:send GUI 'get-mainframe))
        (slider-enabled #f))
   
    
    
    
    
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
   (define editor-canvas (new editor-canvas%	 
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
                   [label (make-object bitmap% "multisensor.png" 'png/alpha)]
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

    
    ;paneel dat de slider en de choices bij elkaar houd
    (define setter_panel (new horizontal-panel%
                              [parent change_panel]
                              [spacing 0]
                              
                              [vert-margin 20]
                              [alignment (list 'left 'center)]
                              ))

     
     
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

    
    (define (compose_set_command choice slider)
      (let ((value (send slider get-value)))
      (format "SET ~a=~a\n" choice value)))
    
    
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
                                                                        (current-steward (util:send majordomo 'get-current-steward)))
                                                                      
                                                                   (if slider-enabled
                                                                          ;stuur commando 
                                                                          (let ((commando (compose_set_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection))))
                                                                                (printcommando (compose_print_command (send choice get-string-selection) (get-right-slider (send choice get-string-selection)))))
                                                                            
                                                                            (send text insert (format ">> Sending ~a to ~a in the ~a ... \n " printcommando device-name current-steward))
                                                                            (util:send logsystem 'device-on current-steward device-name)
                                                                            (let ((answer  (util:send majordomo 'request commando name_of_device)))
                                                                            (send text insert (format ">>>> Answer: ~a " answer))
                                                                             (if (check_ack answer)
                                                                                 ( util:send logsystem 'change-mesurement current-steward name_of_device (send choice get-string-selection) (send (get-right-slider (send choice get-string-selection)) get-value))
                                                                                 'nack)))
                                                                          'done)
                                                                 
                                                       
                                                                   (send editor-canvas set-editor text)
                                                                   ;update info device
                                                                   (update_device_info)
                                                                   ))]))
    
    
    
    
    
    ;return the right slider
    (define (get-right-slider choice)
      (set! slider-enabled #t)
     ( cond ((equal? choice "TXT")
            TXT-slider)
      ((equal? choice "MSI")
       MSI-slider)
      ((equal? choice "LOTEM")
       LOTEM-slider)
      ((equal? choice "HITTEM")
       HITTEM-slider)
      ((equal? choice "LOBRI")
       LOBRI-slider)
   (else HIBRI-slider)))
    
    
    ;stelt een choicebox op met daarin al de keuzes
    
(define choice
      (new choice%  (label "Choice")
           (parent setter_panel)
           (choices (list "TXT" "MSI" "LOTEM" "HITTEM" "LOBRI" "HIBRI"))
           [callback (lambda (c e) (send setter_panel change-children (lambda (x) (list choice (get-right-slider (send c get-string-selection))))))]))
    
    
 
    
    ;verschillende sliders
    ;---------------------
    
    (define TXT-slider (new slider%
                             (label "")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    
    (define MSI-slider (new slider%
                             (label "seconds")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    (define LOTEM-slider (new slider%
                             (label "°c")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    (define HITTEM-slider (new slider%
                             (label "°c")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    
    (define LOBRI-slider (new slider%
                             (label "Lux")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 1)
                             (init-value 0)))
    
    (define HIBRI-slider (new slider%
                             (label "Lux")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 1)
                             (init-value 0)))
    
    
    ;enkel choice box instellen, pas bij selectie komt de slider  
    (send setter_panel change-children (lambda (x) (list choice)))
    
    
(define (update_device_info)
  (send device_info_canvas_info_GET change-children (lambda (x) '()))
  (create_info_panel device_info_canvas_info_GET (split-string-in-list (util:send majordomo 'request "GET\n" name_of_device) #\newline))
  (util:send logsystem 'status-update (util:send majordomo 'get-current-steward) name_of_device))
    
    
    (create_info_panel device_info_canvas_info_GET (split-string-in-list (util:send majordomo 'request "GET\n" name_of_device) #\newline))
     (create_info_panel device_info_canvas_info_DEV (split-string-in-list (util:send majordomo 'request "DEV\n" name_of_device) #\newline))
    
    



    (define (dispatch message)
      (case message 
        ((get-tab) get-tab)
        ((check-policies) check-policies)
        (else (error 'Powerplug-panel "unknown message ~a" message))))
    dispatch
    ))












