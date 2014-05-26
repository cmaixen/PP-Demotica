#lang r5rs


(#%require rnrs/bytevectors-6)


(#%provide (all-defined))

  (define (string->vector string)
    (list->vector (map char->integer (string->list string))))
  
  (define (vector->string vector)
    (list->string  (map integer->char (vector->list vector))))
  
  
        (define (make_frame . arguments)
          (define (loop lst result)
            (if (null? lst)
                (list->vector result)
            (let ((first_arg (car lst)))
              (cond ((integer? first_arg)
                     (loop (cdr lst) (append result (list first_arg))))
                    ((vector? first_arg)
                     (loop (cdr lst) (append result (vector->list first_arg))))
                    ((string? first_arg)
                     (loop (cdr lst) (append result (map char->integer (string->list first_arg)))))
                    (else (display "invalid type to make frame")
                          (newline))))))
          (loop arguments '()))
         
    
  
    (define (vector->bytevector vector)
      (let* ((size (vector-length vector))
             (bytevector (make-bytevector size)))
        (define (loop counter)
          (if (= counter size)
              bytevector
              (begin 
              (bytevector-u8-set! bytevector counter (vector-ref vector counter))
              (loop (+ counter 1)))))
        (loop 0)))
              
          
    (define (bytevector->vector bytevector)
      (let* ((size (bytevector-length bytevector))
             (vector (make-vector size)))
        (define (loop counter)
          (if (= counter size)
              vector
              (begin 
              (vector-set! vector counter (bytevector-u8-ref bytevector counter))
              (loop (+ counter 1)))))
        (loop 0)))
              
  
  ;hulpfunctie om snel info uit verctors te krijge
  (define (vector-loop start end given-vector)
    (define (loop counter final-vector)
      (if (> counter end)
          final-vector
          (begin
            (vector-set! final-vector (- counter start) (vector-ref given-vector (- counter 1)))
            (loop (+ counter 1) final-vector))))
    (loop start (make-vector (+ (- end start) 1))))
  
  
            
  (define (append_string . arguments)
    (define (loop lst result)
      (if (null? lst)
          (list->string result)
          (loop (cdr lst) (append result (string->list (car lst))))))
    (loop arguments '()))
  
  
  
     ;commandos
    (define Transmit_request 16)
    (define Transmit_status 139)
    (define Transmit_answer 144)
    
         (define message-acknowledged 1)
        (define transmit_retry_counter 0)
        (define delivery_status_succes 0)
        (define discovery_status_succes 1)
        