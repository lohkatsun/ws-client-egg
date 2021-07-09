;; a single, very ad-hoc sanity check.
;; for development, use an external websocket client test suite.
(import (chicken process) (chicken condition)
	(chicken tcp) (chicken io) (chicken format)
	(chicken bitwise)
	test to-hex srfi-4
        intarweb base64 simple-sha1 ws-client)

(define port 8080)

(define (expected-sec-websocket-accept key)
  (let* ((s (string->sha1sum (string-append key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
    (base64-encode (hex_to_str (make-string 20) s 0 40))))

;; echo client
(define (client-thunk)
  (let ((conn (ws-connect (sprintf "ws://localhost:~A" port))))
    (recv-message-loop
     conn
     (lambda (m)
       (print m)
       (send-message conn m)))))

;; server
(define (handshake-ping-die)
  (let*-values (((li) (tcp-listen port))
		((pid) (process-fork client-thunk))
		((i o) (tcp-accept li))
		((req) (read-request i))
		((key) (expected-sec-websocket-accept
			(header-value 'sec-websocket-key (request-headers req)))))
    ;; server handshake
    (write-response
     (make-response
      port: o
      code: 101
      reason: "Switching Protocols"
      headers: (headers `((upgrade #("websocket" raw))
			  (connection upgrade)
			  (sec-websocket-accept #(,key raw))))))
    ;; send a single ping frame, and then a close-connection:
    (write-u8vector #u8(137 0) o)
    (write-u8vector #u8(136 2 3 242) o)
    (let ((ret (read-u8vector #f i)))
      ;; clean up
      (close-input-port i)
      (close-output-port o)
      (process-signal pid)
      (tcp-close li)
      ;; check return value: a pong frame and a clean close
      (and (eq? (u8vector-ref ret 0) 138)
	   (eq? (u8vector-ref ret 1) 128)
	   (eq? (u8vector-ref ret 6) 136)
	   (eq? (bitwise-xor (u8vector-ref ret 8) (u8vector-ref ret 12)) 3)
	   (eq? (bitwise-xor (u8vector-ref ret 9) (u8vector-ref ret 13)) 232)))))

(test #t (handshake-ping-die))
(test-exit)


