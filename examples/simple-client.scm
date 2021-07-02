(import ws-client (chicken io) (chicken format))

;; connects to localhost:9001 and sends each line read from stdin
;; until an empty line is encountered.

(define conn (ws-connect "ws://localhost:9001"))

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
