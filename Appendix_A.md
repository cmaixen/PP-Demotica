#Appendix A: Apparaat Communicatieprotocol Scenario



    ;; Ik vraag een input/output port aan het steward ADT ;; voor de slaapkamer, om met een 	specifieke lamp te ;; 'praten'.		> (define slaapkamer-hoofdlamp-port	(slaapkamer-steward 'open-device-io-port 'hoofdlamp))	
	;; Dit zorgt ervoor dat ik niet	;; 'slaapkamer-hoofdlamp-port' moet heralen bij elke ;; (read) en (write) commando.		> (current-input-port slaapkamer-hoofdlamp-port)		> (current-output-port slaapkamer-hoofdlamp-port)	
	;; Brandt het licht in de slaapkamer?		> (write '(GET POW) slaapkamer-hoofdlamp-port) 		> (read)'(ACK (POW OFF))
			;; Een vraag of antwoord is een lijst van symbolen,	;;  met het eerste element een identificatie en met	;;  (PARAMETER . WAARDEN) lijsten als volgende elementen.	;; Zet het licht aan.		> (write '(PUT (POW ON)) slaapkamer-hoofdlamp-port)
			;; Een commando kan een antwoord veroorzaken vanuit het	;;  apparaat, zo kan je nagaan of het commando goed is	;;  ontvangen en afgehandeld.		> (read)		'(ACK (POW ON))
			;; Bij een mogelijk probleem, bijvoorbeeld een typfout, ;; krijg je het volgende antwoord.		> (write '(PPUT OOPS TYPFOUT) slaapkamer-hoofdlamp-port) '(NACK (PPUT OOPS TYPFOUT))	;; Sommige commandos veroorzaken een gebundeld antwoord.		> (write '(GET ALL))		'(ACK (POW ON) (LOAD 62Watt) (WORK 12.60kWh) (TEMP 21.1C)      	(FREQ 50.00Hz) (VRMS 230V) (IRMS 10mA))
      		;; Het volgende is _niet verplicht_,	;; maar is een interessante uitbreiding	;; Zend me (asynchroon) een berichtje als het verbruik ;; de grens van 15 kWh overschrijdt.		> (write '(METER (WORK 15 MSG)))
			;; Een (read) zal hier blijven wachten tot er een berichtje ;; ontvangen is, wat lang kan 		duren.	;; We gebruiken daarom 'thread' van Racket om dit concurrent ;; te laten verlopen. Lees 		goed de Racket documentatie	;;  vooraleer je hieraan begint.		> (thread (lambda () ! (display (read)))) 		> (display "waiting ...") waiting...			(WORK)