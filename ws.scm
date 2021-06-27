(module
 ws (ws-connect
     ws-connection?

     recv-message recv-message-loop
     send-message send-text-message send-binary-message

     ws-message?
     message-type message-data* message-data message-size

     ;; low-level interface:

     opcode->optype optype->opcode
     ;; base-protocol-opcode? control-opcode? data-opcode?
     ;; opcode-connection-close? opcode->continuation?

     reason->close-code close-code->reason
     ;; valid-close-code?

     ws-frame?
     frame-fin frame-rsv frame-opcode frame-optype frame-mask?
     frame-payload-length frame-payload-data

     send-frame recv-frame)

 (import scheme (chicken base) (chicken type) (chicken string)
	 (chicken condition) (chicken io) (chicken format)
	 (chicken foreign) (chicken blob) (chicken bitwise)
	 (chicken random) (chicken tcp)
	 srfi-4 openssl uri-common intarweb base64 simple-sha1)

 (include "ws-utf8")
 (include "ws-permessage-deflate")

 ;; error handling

 (define ws-error error)

 (define (ws-exn msg)
   (signal (make-composite-condition
	    (make-property-condition 'websocket)
	    (make-property-condition 'exn 'message msg))))

 (define (ws-fail rsn msg)
   (signal (make-composite-condition
	    (make-property-condition 'websocket)
	    (make-property-condition 'fail 'reason rsn 'message msg))))

 ;; opcodes & close codes

 (: opcode->optype (fixnum --> symbol))
 (define (opcode->optype o)
   (case o
     ((#x0) 'continuation)
     ((#x1) 'text)
     ((#x2) 'binary)
     ;; #x3 - #x7 reserved data opcodes
     ((#x8) 'connection-close)
     ((#x9) 'ping)
     ((#xa) 'pong)
     ;; #xb - #xf reserved control opcodes
     (else (ws-exn "unrecognised opcode"))))

 (: optype->opcode (symbol --> fixnum))
 (define (optype->opcode t)
   (case t
     ('continuation     #x0)
     ('text             #x1)
     ('binary           #x2)
     ('connection-close #x8)
     ('ping             #x9)
     ('pong             #xa)
     (else (ws-exn "unrecognised optype"))))

 (: base-protocol-opcode? (fixnum --> boolean))
 (: control-opcode? (fixnum --> boolean))
 (: data-opcode? (fixnum --> boolean))

 (: opcode-connection-close? (fixnum --> boolean))
 (: opcode-continuation? (fixnum --> boolean))

 (define (base-protocol-opcode? o) (case o ((#x0 #x1 #x2 #x8 #x9 #xa) #t) (else #f)))
 (define (control-opcode? o) (and (< 7 o) (< o 16)))
 (define (data-opcode? o) (and (<= 0 o) (< o 8)))

 (define (opcode-connection-close? o) (eq? 'connection-close (opcode->optype o)))
 (define (opcode-continuation? o) (eq? 'continuation (opcode->optype o)))

 (: reason->close-code (symbol --> u8vector))
 (define (reason->close-code r)
   (case r
     ('normal-closure             #u8(3 232)) ;; 1000
     ('going-away                 #u8(3 233)) ;; 1001
     ('protocol-error             #u8(3 234)) ;; 1002
     ('unsupported-data           #u8(3 235)) ;; 1003
     ;; 1004 reserved
     ('no-status-rcvd             #u8(3 237)) ;; 1005
     ('abnormal-closure           #u8(3 238)) ;; 1006
     ('invalid-frame-payload-data #u8(3 239)) ;; 1007
     ('policy-violation           #u8(3 240)) ;; 1008
     ('message-too-big            #u8(3 241)) ;; 1009
     ('mandatory-ext              #u8(3 242)) ;; 1010
     ('internal-server-error      #u8(3 243)) ;; 1011
     ('tls-handshake              #u8(3 247)) ;; 1015
     (else (ws-exn (sprintf "unsupported reason for connection close ~A" r)))))

 (: close-code->reason (fixnum --> symbol))
 (define (close-code->reason c)
   (case c
     ((1000) 'normal-closure)
     ((1001) 'going-away)
     ((1002) 'protocol-error)
     ((1003) 'unsupported-data)
					; 1004 reserved
     ((1005) 'no-status-rcvd)
     ((1006) 'abnormal-closure)
     ((1007) 'invalid-frame-payload-data)
     ((1008) 'policy-violation)
     ((1009) 'message-too-big)
     ((1010) 'mandatory-ext)
     ((1011) 'internal-server-error)
     ((1015) 'tls-handshake)
     (else (ws-exn (sprintf "unsupported close code ~A" c)))))

 (: valid-close-code? (u8vector --> boolean))
 (define (valid-close-code? c)
   (let ((code (+ (* 256 (u8vector-ref c 0)) (u8vector-ref c 1))))
     (case code
       ((1000 1001 1002 1003 1007 1008 1009 1010 1011) #t)
       (else (and (< 2999 code) (< code 5000))))))

 (: valid-close-frame-payload? (u8vector #!optional integer --> boolean))
 (define (valid-close-frame-payload? data #!optional (len (u8vector-length data)))
   (cond
    ((< 1 len)
     (and (valid-close-code? data)
	  (= 0 (utf8d data (- len 2) 2 0))))
    ((= 0 len) #t)
    (else #f)))

 ;; websocket frame record

 (: make-ws-frame (boolean fixnum fixnum boolean integer u8vector --> (struct ws-frame)))
 (: ws-frame? (* --> boolean))
 (: frame-fin ((struct ws-frame) --> boolean))
 (: frame-rsv ((struct ws-frame) --> fixnum))
 (: frame-opcode ((struct ws-frame) --> fixnum))
 (: frame-optype ((struct ws-frame) --> symbol))
 (: frame-mask? ((struct ws-frame) --> boolean))
 (: frame-payload-length ((struct ws-frame) --> integer))
 (: frame-payload-data ((struct ws-frame) --> u8vector))

 (define-record-type ws-frame
   (make-ws-frame fin rsv op mask len data)
   ws-frame?
   (fin  frame-fin)
   (rsv  frame-rsv)
   (op   frame-opcode)
   ;; we only remember whether a frame is masked; if it is, the
   ;; payload is masked/unmasked quietly when the frame is processed.
   (mask frame-mask?)
   (len  frame-payload-length)
   (data frame-payload-data))

 (define (frame-optype f)
   (opcode->optype (frame-opcode f)))

 (define-record-printer (ws-frame f out)
   (fprintf out "#<ws-frame fin=~A rsv~A op=~A mask=~A payload=~A (~A)>"
	    (frame-fin f) (frame-rsv f) (frame-optype f) (frame-mask? f)
	    (if (< 12 (frame-payload-length f)) "..." (frame-payload-data f))
	    (frame-payload-length f)))

 ;; websocket message record
 (: make-ws-message (symbol (list-of (struct ws-frame)) --> (struct ws-message)))
 (: make-ws-message* (symbol (list-of (struct ws-frame)) u8vector --> (struct ws-message)))
 (: ws-message? (* --> boolean))
 (: message-type ((struct ws-message) --> symbol))
 (: message-frames ((struct ws-message) --> (list-of (struct ws-frame))))
 (: message-data* ((struct ws-message) --> u8vector))
 (: message-data ((struct ws-message) --> (or string blob)))
 (: message-size ((struct ws-message) --> integer))

 (define-record-type ws-message
   (make-ws-message* type frames data)
   ws-message?
   (type message-type)
   (frames message-frames)
   (data message-data*))

 (: conc-frame-payloads (u8vector integer (list-of (struct ws-frame)) -> undefined))
 (define (conc-frame-payloads buf start frames)
   (if (not (eq? '() frames))
       (let* ((f (car frames))
	      (len (frame-payload-length f)))
	 ;; this should be u8vector-copy!, but srfi-66 seems to be broken
	 ((foreign-lambda* void ((u8vector trg) (u8vector src) (size_t start) (size_t len))
			   "memcpy(trg+start, src, len);") buf (frame-payload-data f) start len)
					;	 (u8vector-copy! (frame-payload-data f) buf start len)
	 (conc-frame-payloads buf (+ start len) (cdr frames)))))

 (define (make-ws-message type frames)
   (let* ((len (foldl (lambda (a f) (+ a (frame-payload-length f))) 0 frames))
	  (buf (make-u8vector len 0)))
     (conc-frame-payloads buf 0 frames)
     (make-ws-message* type frames buf)))

 (define (message-data m)
   (case (message-type m)
     ('text   (blob->string (u8vector->blob/shared (message-data* m))))
     ('binary (u8vector->blob/shared (message-data* m)))
     (else (ws-error 'message-data "unrecognised message type"))))

 (define (message-size m)
   (u8vector-length (message-data* m)))

 (define-record-printer (ws-message m out)
   (fprintf out "#<ws-message type=~A data=~A>"
	    (message-type m)
	    (if (< 12 (message-size m)) "..." (message-data m))))

 ;; websocket connection record

 (: make-ws-connection (input-port output-port (list-of (struct ws-extension))
				   --> (struct ws-connection)))
 (: ws-connection? (* --> boolean))
 (: in-port ((struct ws-connection) --> input-port))
 (: out-port ((struct ws-connection) --> output-port))
 (: extensions ((struct ws-connection) --> (list-of (struct ws-extension))))

 (define-record-type ws-connection
   (make-ws-connection i o exts)
   ws-connection?
   (i in-port)
   (o out-port)
   (exts extensions))

 ;; websocket uri validation

 (: ws-uri (string --> (struct uri-common)))
 (define (ws-uri uri)
   (let ((wsuri (handle-exceptions
		 _ (ws-error 'ws-uri "invalid websocket URI (websocket URI must be absolute)")
		 (absolute-uri uri))))
     (update-uri wsuri port:
		 ;; explicitly specify default ports
		 (case (uri-scheme wsuri)
		   ((ws)  (or (uri-port wsuri) 80))
		   ((wss) (or (uri-port wsuri) 443))
		   (else  (ws-error 'ws-uri "invalid websocket URI (scheme must be one of 'ws or 'wss)"))))))

 ;; websocket opening handshake

 (: send-client-opening-handshake (input-port (struct uri-common) string -> output-port))
 (define (send-client-opening-handshake o wsuri key)
   (let* ((host (uri-host wsuri))
	  (port (uri-port wsuri))
	  (req (make-request
		uri: wsuri
		port: o
		headers: (headers
			  `((host (,host . ,port))
			    (upgrade #("websocket" raw))
			    (connection #("upgrade" raw))
			    (sec-websocket-key #(,key raw))
			    (sec-websocket-version #("13" raw)))))))
     ;; apparently write-request might modify out-port, so we
     ;; return this
     (request-port (write-request req))))

 (: expected-sec-websocket-accept (string --> string))
 (define (expected-sec-websocket-accept key)
   (let* ((s (string->sha1sum (string-append key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
     ;; convert the string representation of the SHA1 hash into a
     ;; string of the actual bytes
     ((foreign-lambda* void ((blob s))
		       "
unsigned char *ha = s;
unsigned char *hb = ha;
for (size_t i = 0; i < 20; ++i) {
  *ha = 16*(*hb - ('a' <= *hb ? 87 : 48)); ++hb;
  *ha += *hb - ('a' <= *hb ? 87 : 48); ++hb;
  ++ha;
}
") s)
     ;; base64-encode the bytestring
     (base64-encode (substring s 0 20))))

 (: read-server-opening-handshake (input-port string -> *))
 (define (read-server-opening-handshake i key)
   (let* ((res (read-response i))
	  (h (response-headers res)))
     (case (response-code res)
       ((101)
	;; validate server handshake
	(if (not (and
		  ;; connection: upgrade
		  (memq 'upgrade (header-values 'connection h))
		  ;; upgrade: websocket
		  (header-value 'upgrade h)
		  (string-ci=? "websocket" (car(header-value 'upgrade h)))
		  ;; sec-websocket-accept: (base64 (SHA1 (key + magic)))
		  (string=? (expected-sec-websocket-accept key)
			    (header-value 'sec-websocket-accept h))))
	    (ws-fail 'protocol-error "invalid upgrade headers"))
	#t)
       ;; for responses other than 101, do nothing except report it to
       ;; the user.
       (else (ws-error 'ws-connect
		       (sprintf "opening handshake unsuccessful: ~A ~A"
				(response-code res) (response-reason res)))))))



 (: ws-connect (string -> (struct ws-connection)))
 (define (ws-connect uri)
   (let*-values
       (((wsuri) (ws-uri uri))
	((host) (uri-host wsuri))
	((port) (uri-port wsuri))
	((key) (base64-encode (random-bytes (make-string 16))))
	;; open TCP connection to server
	((i o)
	 (if (eq? 'wss (uri-scheme wsuri))
	     (ssl-connect* hostname: host port: port)
	     (tcp-connect host port)))
	((o*) (send-client-opening-handshake o wsuri key)))
     (if (read-server-opening-handshake i key)
	 (make-ws-connection i o* '()))))


 ;; send a single websocket frame
 (: send-frame ((struct ws-connection) symbol u8vector
		#!optional integer boolean boolean fixnum -> undefined))
 (define (send-frame conn op data
		     #!optional (len (u8vector-length data)) (mask #t) (fin #t) (rsv 0))
   (let* ((buf (make-u8vector (+ 14 len) 0))
	  (size ((foreign-lambda* size_t ((u8vector u)
					  (byte op) (u8vector data) (unsigned-integer64 len)
					  (bool mask) (bool fin) (byte rsv)
					  (u8vector key))
				  "
unsigned char *u_orig = u;
*(u++)=((8*fin+(rsv&7))<<4)+(op&15);
*u=128*mask;
size_t offset;
if (len < 126)        { *u += len; offset = 0; }
else if (len < 65536) { *u += 126; offset = 2; }
else                  { *u += 127; offset = 8; }

unsigned char *v = u + offset;
size_t l = len;
for (; v > u; --v) {
  *v = (l & 255);
  l >>= 8;
}

u = u + offset + 1;

if (mask) {
  memcpy(u, key, 4);
  u += 4;
  memcpy(u, data, len);
  for (size_t i = 0; i < len; ++i) *(u++) ^= key[i%4];
} else {
  memcpy(u, data, len);
  u += len;
}

C_return(u-u_orig);
"
				  )
		 buf (optype->opcode op) data len mask
		 fin rsv (blob->u8vector/shared (random-bytes (make-blob 4))))))
     (write-u8vector buf (out-port conn) 0 size)))

 ;; (these are just different shorthands for send-frame until
 ;; fragmenting outgoing messages if implemented:)

 (: send-message ((struct ws-connection) symbol u8vector #!optional integer --> undefined))
 (: send-text-message ((struct ws-connection) string -> undefined))
 (: send-binary-message ((struct ws-connection) blob -> undefined))

 (define (send-message conn type data #!optional (len (u8vector-length data)))
   (send-frame conn type data len))

 (define (send-text-message conn data)
   (send-message conn 'text (blob->u8vector/shared (string->blob data))))

 (define (send-binary-message conn data)
   (send-message conn 'binary (blob->u8vector/shared data)))

 ;; these are like read-u8vector and read-u8vector!, except the length
 ;; option is not optional and they throw an exception when the hit an
 ;; eof earlier than expected.

 (: read-full-u8vector! (integer u8vector #!optional input-port integer -> u8vector))
 (define (read-full-u8vector! len buf
			      #!optional (port (current-input-port)) (start 0))
   (let ((ret (read-u8vector! len buf port start)))
     (if (< len ret)
	 (ws-exn "connection failed")
	 buf)))

 (: read-full-u8vector (integer #!optional input-port integer -> u8vector))
 (define (read-full-u8vector len
			     #!optional (port (current-input-port)) (start 0))
   (read-full-u8vector! len (make-u8vector len) port start))

 (: interpret-b0 (fixnum --> boolean fixnum fixnum))
 (define (interpret-b0 b)
   (if (eof-object? b)
       (ws-exn "connection lost"))
   (let* ((fin (< 0 (bitwise-and b 128)))
	  (rsv (arithmetic-shift (bitwise-and b 112) -4))
	  (op  (bitwise-and b 15)))
     (if (< 0 rsv)
	 (ws-fail 'protocol-error "unsupported RSV bits"))
     (if (not (base-protocol-opcode? op))
	 (ws-fail 'protocol-error "unsupported opcode"))
     (values fin rsv op)))

 (: interpret-b1 (fixnum --> boolean fixnum))
 (define (interpret-b1 b)
   (if (eof-object? b)
       (ws-exn "connection lost"))
   (let* ((mask (< 0 (bitwise-and b 128)))
	  (len0   (bitwise-and b 127)))
     (values mask len0)))

 (: mask-buffer! (integer u8vector integer u8vector -> undefined))
 (define (mask-buffer! len buf start key)
   ((foreign-lambda* void ((size_t len) (u8vector buf) (size_t start) (u8vector key))
		     "
buf += start;
for (size_t i = 0; i < len; ++i) *(buf++) ^= key[i%4];
"
		     ) len buf start key))

 (: read-payload-length* (input-port fixnum integer -> integer))
 (define (read-payload-length* i rem temp)
   (if (= rem 0) temp
       (let ((b (read-byte i)))
	 (if (eof-object? b)
	     (ws-exn "connection lost"))
	 (read-payload-length* i (- rem 1) (+ (* 256 temp) b)))))

 (: read-payload-length (input-port fixnum -> integer))
 (define (read-payload-length i len0)
   (case len0
     ((127) (read-payload-length* i 8 0))
     ((126) (read-payload-length* i 2 0))
     (else len0)))

 ;; read a single websocket frame; raise signal if protocol violated
 (: recv-frame ((struct ws-connection) -> (struct ws-frame)))
 (define (recv-frame conn)
   (let*-values
       (((i) (in-port conn))
	((b0) (read-byte i))
	((fin rsv op) (interpret-b0 b0))
	((b1) (read-byte i))
	((mask len0) (interpret-b1 b1)))
     (if (control-opcode? op)
	 (cond
	  ((not fin) (ws-fail 'protocol-error "fragmented control frame"))
	  ((< 125 len0) (ws-fail 'protocol-error "control frame with payload length > 125"))))
     (let* ((len (read-payload-length i len0))
	    (key (if mask (read-full-u8vector 4 i) #f))
	    (buf (read-full-u8vector len i))
	    (f (make-ws-frame fin rsv op mask len buf)))
       f)))

 (: recv-message-loop ((struct ws-connection) ((struct ws-message) -> *) -> undefined))
 (define (recv-message-loop conn handler)
   (let ((m (recv-message conn)))
     (if (ws-message? m) (begin
			   (handler m)
			   (recv-message-loop conn handler)))))


 (: recv-message ((struct ws-connection) -> (or false (struct ws-message))))
 (: recv-message* ((struct ws-connection) symbol (list-of (struct ws-frame)) integer
		   -> (or false (struct ws-message))))
 (define (recv-message conn)
   (condition-case
    (recv-message* conn 'none '() 0)
    (e (websocket fail)
       (print (get-condition-property e 'fail 'message))
       (send-frame conn
		   'connection-close
		   (reason->close-code (get-condition-property e 'fail 'reason))))))

 (define (recv-message* conn type frames utf-st8)
   ;; (define (recv-message* conn type count)
   (let* ((f (recv-frame conn))
	  (op (frame-optype f))
	  (data (frame-payload-data f))
	  (len (frame-payload-length f))
	  (utf-upd8
	   (if (or (eq? 'text op) (and (eq? 'continuation op) (eq? 'text type)))
	       (utf8d data len 0 utf-st8) utf-st8)))
     (if (= 1 utf-upd8)
	 (ws-fail 'protocol-error "text message contains invalid utf-8 (2)"))
     ;; handle continuation & control frames until a complete message
     ;; can be assembled
     (case op
       ;; text/bianry
       ((text binary)
	(cond
	 ((not (frame-fin f))
	  ;;(recv-message* conn op (+ count 1)))
	  (recv-message* conn op (cons f frames) utf-upd8))
	 ((not (eq? 'none type))
	  (ws-fail 'protocol-error "fragments out of order"))
	 ((and (eq? 'text op) (< 0 utf-upd8))
	  (ws-fail 'protocol-error "text message contains invalid utf-8"))
	 (else
	  (make-ws-message op (reverse (cons f frames))))))
       ;; continuation
       ((continuation)
	(cond
	 ((eq? 'none type) (ws-fail 'protocol-error "nothing to continue"))
	 ((frame-fin f)
	  (if (and (eq? 'text type) (< 0 utf-upd8))
	      (ws-fail 'protocol-error "text message contains invalid utf-8")
	      (make-ws-message type (reverse (cons f frames)))))
	 (else (recv-message* conn type (cons f frames) utf-upd8))))
       ;; ping/pong
       ((ping)
	(send-frame conn 'pong data len)
	(recv-message* conn type frames utf-upd8))
       ((pong)
	(recv-message* conn type frames utf-upd8))
       ((connection-close)
	(if (valid-close-frame-payload? data len)
	    (send-frame conn 'connection-close data len)
	    (ws-fail 'protocol-error "invalid close frame payload"))))))
 )
