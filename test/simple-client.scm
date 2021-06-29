(import ws (chicken io) (chicken format))

;; connects to localhost:9001 and echoes back every text message
;; received from the server

(define conn (ws-connect "ws://localhost:9001" (list swapin swapout)))

(send-text-message conn (read-line))


(recv-message-loop
 conn
 (lambda (m)
   (let ((d (message-data m)))
     (if (eq? 'text (message-type m))
	 (begin
	   (printf "rcvd: ~A\n" d)
	   (let ((l (read-line)))
	     (if (equal? "" l)
		 (ws-close conn 'normal-closure)
		 (send-text-message conn l))))))))
