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

; Beschrijving: ADT GUI_analysticstab ; Argumenten: Majordomo-object, GUI-object
;               Het ADT is een voorgeschreven panel met dat grafieken weergeeft over de toestellen
;
; Output: gui_graph object
;
; Messages:
;
; - get-analysticstab: argumenten: geen
;          output: panel%
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(require plot)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(provide make_GUI_analysticstab)


(define (make_GUI_analysticstab GUI)
  
  
  (define (get-analysticstab) analysticstab)
  
  (define analysticstab (new vertical-panel% 
                        [parent (util:send GUI 'get-tabpanel)]
                        [alignment (list 'center 'center)]
                        ))
  
  (define (generate-list-graph givenlst)
    (define (loop lst counter result)
      (if (= counter 0)
          result
          (loop (cdr lst) (- counter 1) (cons (vector counter (car lst) ) result))))
    (loop givenlst 24 '()))
  
  
  (define graph-light (new canvas%	 
                     [parent analysticstab]
                     [paint-callback
                      (lambda (canvas dc)
                        (plot/dc (discrete-histogram
                                  (let* ((input (open-input-file light-graph-file))
                                         
                                         (input (read input)))
                                    
                                    
                                    (generate-list-graph input))
                                  #:x-min 0	 	 	 	 
                                  #:x-max 24
                                  #:y-min 0	 	 	 	 
                                  #:y-max 1200
                                  #:color 6 #:label "Average mesurement of Lightswitches")
                                  #:y-label "Average Mesurement (Watt)"
                                  #:x-label "How long ago (Hours)"
                                 dc
                                 
                                 1
                                 1
                                 300
                                 300
                                
                                 ))]))
  
    (define graph-temp (new canvas%	 
                     [parent analysticstab]
                     [paint-callback
                      (lambda (canvas dc)
                        (plot/dc (discrete-histogram
                                  (let* ((input (open-input-file temp-graph-file))
                                         
                                         (input (read input)))
                                    
                                    
                                    (generate-list-graph input))
                                  #:x-min 0	 	 	 	 
                                  #:x-max 24
                                  #:y-min 0
                                   #:y-max 35
                                  
                                  #:color 4 #:label "Average mesurement of Temperaturesensors")
                                  #:y-label "Average Mesurement (°C)"
                                  #:x-label "How long ago (Hours)"
                                 dc
                                 
                                 300
                                 1
                                 300
                                 300
                                
                                 ))]))
  
  
  (thread (lambda ()  
            (do ([i 1 (+ i 1)])
              ((= i 0) (print "done"))
              
              (sleep 1)
              (print "Graph refresh")
              (newline)
           
                 (send graph-light refresh)
              (send graph-temp refresh))))
  
  
  (define (dispatch message)
    (case message 
      ((get-analysticstab) get-analysticstab)
      (else (error 'analysticstab "unknown message ~a" message))))
  
  dispatch
  )



