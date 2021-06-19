(import ws (chicken format))

;; connects to localhost:9001 and echoes back every text message
;; received from the server

(define conn (ws-connect "ws://localhost:9001"))

(recv-message-loop
 conn
 (lambda (m)
   (let ((d (message-data m)))
     (if (eq? 'text (message-type m))
	 (begin
	   (printf "rcvd: ~A\n" d)
	   (send-text-message conn d))))))
