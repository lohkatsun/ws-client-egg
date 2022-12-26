(module
 ws-client
 (ws-connect
  ws-connection?

  ws-close

  recv-message recv-message-loop
  send-message send-text-message send-binary-message

  ws-message?
  ;; make-ws-message
  message-type
  message-frames
  message-data* message-data message-size

  ;; extensions
  permessage-deflate

  ;; low-level interface:
  opcode->optype optype->opcode
  ;; base-protocol-opcode? control-opcode? data-opcode?
  ;; opcode-connection-close? opcode->continuation?

  reason->close-code close-code->reason
  ;; valid-close-code?

  make-ws-frame
  ws-frame? data-frame? control-frame?
  frame-fin? frame-rsv frame-rsv-bit
  frame-opcode frame-optype frame-mask?
  frame-payload-length frame-payload-data

  send-frame recv-frame

  ;; extension interface (used only by permessage-deflate.scm)

  ;; make-ws-extension
  ;; extension-desc extension-token extension-params
  ;; extension-param-value
  ;; extension-desc->string
  ;; extension-desc*->string
  ;; string->extension-desc
  ;; string->extension-desc*
  ;; extensions

  ;; valid-rsv-set!
  ;; valid-rsv-set-bit!
  ;; valid-rsv-unset-bit!
  ;; frame-rsv-set!
  ;; frame-rsv-set-bit!
  ;; frame-rsv-unset-bit!
  )

 (import scheme (chicken base) (chicken type)
	 (chicken memory) (chicken condition)
	 (chicken string) (chicken io) (chicken format)
	 (chicken foreign) (chicken blob) (chicken bitwise)
	 (chicken random) (chicken tcp)
	 srfi-1 srfi-4 foreigners to-hex
	 openssl uri-common intarweb base64 simple-sha1)

 (define-type ws-connection (struct ws-connection))
 (define-type ws-frame (struct ws-frame))
 (define-type ws-message (struct ws-message))

 (include "utf8")
 (include "extensions")
 (include "permessage-deflate")

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
     ;; TODO: utf-8 validation here happens before any extensions
     ;; might process a close frame. PMCEs operate only on data frames
     ;; so this is okay now, but may be a source of problems later.
     (and (valid-close-code? data) (utf-valid8 data (- len 2) 2)))
    ((= 0 len) #t)
    (else #f)))

 ;; websocket frame record

 (: make-ws-frame (boolean fixnum fixnum boolean integer u8vector --> ws-frame))
 (: ws-frame? (* --> boolean))
 (: frame-fin? (ws-frame --> boolean))
 (: frame-rsv (ws-frame --> fixnum))
 (: frame-rsv-bit (ws-frame fixnum --> boolean))
 (: frame-rsv-set! (ws-frame fixnum -> undefined))
 (: frame-rsv-set-bit! (ws-frame fixnum -> undefined))
 (: frame-rsv-unset-bit! (ws-frame fixnum -> undefined))
 (: frame-opcode (ws-frame --> fixnum))
 (: frame-optype (ws-frame --> symbol))
 (: frame-mask? (ws-frame --> boolean))
 (: frame-payload-length (ws-frame --> integer))
 (: frame-payload-data (ws-frame --> u8vector))

 (: data-frame? (ws-frame --> boolean))
 (: control-frame? (ws-frame --> boolean))

 (define-record-type ws-frame
   (make-ws-frame fin rsv op mask len data)
   ws-frame?
   (fin  frame-fin?)
   (rsv  frame-rsv frame-rsv-set!)
   (op   frame-opcode)
   ;; we only remember whether a frame is masked; if it is, the
   ;; payload is masked/unmasked quietly when the frame is processed.
   (mask frame-mask?)
   (len  frame-payload-length)
   (data frame-payload-data))

 (define (frame-rsv-bit f b)
   (= b (bitwise-and b (frame-rsv f))))

 (define (frame-rsv-set-bit! f b)
   (frame-rsv-set! f (bitwise-ior b (frame-rsv f))))

 (define (frame-rsv-unset-bit! f b)
   (frame-rsv-set! f (bitwise-and b (bitwise-not (frame-rsv f)))))

 (define (frame-optype f)
   (opcode->optype (frame-opcode f)))

 (define-record-printer (ws-frame f out)
   (fprintf out "#<ws-frame fin=~A rsv=~A op=~A mask=~A payload=~A (~A)>"
	    (frame-fin? f) (frame-rsv f) (frame-optype f) (frame-mask? f)
	    (if (< 12 (frame-payload-length f)) "..." (frame-payload-data f))
	    (frame-payload-length f)))

 (define (data-frame? f) (data-opcode? (frame-opcode f)))
 (define (control-frame? f) (control-opcode? (frame-opcode f)))

 ;; websocket message record
 (: frames->message ((list-of ws-frame) --> ws-message))
 (: make-ws-message* (symbol (list-of ws-frame) u8vector --> ws-message))
 (: make-ws-message (symbol u8vector --> ws-message))
 (: ws-message? (* --> boolean))
 (: message-type (ws-message --> symbol))
 (: message-frames (ws-message --> (list-of ws-frame)))
 (: message-frames-set! (ws-message (list-of ws-frame) -> *))
 (: message-data* (ws-message --> u8vector))
 (: message-data*-set! (ws-message u8vector -> undefined))
 (: message-data (ws-message --> (or string blob)))
 (: message-size (ws-message --> integer))

 (define-record-type ws-message
   (make-ws-message* type frames data)
   ws-message?
   (type message-type)
   (frames message-frames message-frames-set!)
   (data message-data* message-data*-set!))

 (define (make-ws-message type data) (make-ws-message* type '() data))

 (: conc-frame-payloads (u8vector integer (list-of ws-frame) -> undefined))
 (define (conc-frame-payloads buf start frames)
   (if (not (eq? '() frames))
       (let* ((f (car frames))
	      (len (frame-payload-length f)))
	 (move-memory! (frame-payload-data f) buf len 0 start)
	 (conc-frame-payloads buf (+ start len) (cdr frames)))))/

 (define (frames->message frames)
   (let* ((type (frame-optype (car frames)))
	  (len (foldl (lambda (a f) (+ a (frame-payload-length f))) 0 frames))
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
 (: make-ws-connection (input-port output-port fixnum (list-of ws-extension)
				   --> ws-connection))
 (: ws-connection? (* --> boolean))
 (: in-port (ws-connection --> input-port))
 (: out-port (ws-connection --> output-port))
 (: valid-rsv (ws-connection --> fixnum))
 (: valid-rsv-set! (ws-connection fixnum -> undefined))
 (: valid-rsv-set-bit! (ws-connection fixnum -> undefined))
 (: valid-rsv-unset-bit! (ws-connection fixnum -> undefined))
 (: extensions (ws-connection --> (list-of ws-extension)))
 (: extensions-set! (ws-connection (list-of ws-extension) -> undefined))
 (define-record-type ws-connection
   (make-ws-connection i o rsv exts)
   ws-connection?
   (i in-port)
   (o out-port)
   (rsv valid-rsv valid-rsv-set!)
   (exts extensions extensions-set!))

 (define (valid-rsv-set-bit! conn b)
   (valid-rsv-set! conn (bitwise-ior b (valid-rsv conn))))

 (define (valid-rsv-unset-bit! conn b)
   (valid-rsv-set! conn (bitwise-and b (bitwise-not (valid-rsv conn)))))


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

 (: send-client-opening-handshake (output-port (struct uri-common) string (list-of ws-extension) #!optional (list-of symbol)
                                               -> output-port))
 (define (send-client-opening-handshake o wsuri key exts #!optional (flags '()))
   (let* ((strip-host (member 'strip-host flags))
          (host (uri-host wsuri))
	  (port (uri-port wsuri))
	  (ext-offer (extension-desc*->string (join (map extension-desc* exts))))
	  (h (headers
	      `((host (,host . ,port))
		(upgrade #("websocket" raw))
		(connection #("upgrade" raw))
		(sec-websocket-key #(,key raw))
		(sec-websocket-version #("13" raw))
		(sec-websocket-extensions #(,ext-offer raw))))))
     ;; apparently write-request might modify out-port, so we
     ;; return this
     (request-port (write-request (make-request
                                   uri: (if strip-host
                                            (update-uri wsuri host: #f port: #f scheme: #f)
                                            wsuri)
                                   port: o
                                   headers: h)))))

 (: expected-sec-websocket-accept (string --> string))
 (define (expected-sec-websocket-accept key)
   ;; concatenate string with magic, then base64-encode its SHA1 hash
   (let* ((s (string->sha1sum (string-append key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
     (base64-encode (hex_to_str (make-string 20) s 0 40))))

 (: read-server-opening-handshake (input-port output-port string (list-of ws-extension)
					      -> (or ws-connection (struct response))))
 (define (read-server-opening-handshake i o key exts)
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
		  (string-ci=? "websocket" (car (header-value 'upgrade h)))
		  ;; sec-websocket-accept: (base64 (SHA1 (key + magic)))
		  (string=? (expected-sec-websocket-accept key)
			    (header-value 'sec-websocket-accept h))))
	    (ws-fail 'protocol-error "invalid upgrade headers"))
	;; negotiate extensions
	(let ((conn (make-ws-connection i o 0 exts))
	      (edl (string->extension-desc*
		    (string-intersperse (header-values 'sec-websocket-extensions h) ","))))
	  ;; use only those extensions which accept the server parameters
	  (extensions-set! conn
			   (filter (lambda (e) (extension-in-desc* e edl)) exts))
	  ;; initialise each extensions
	  (for-each
	   (lambda (e)
	     ((extension-init e) (extension-params e) conn))
	   (extensions conn))
	  ;; finally, return the connection record
	  conn))
       ;; for responses other than 101, return the HTTP response for
       ;; the caller to process accordingly e.g. by following a
       ;; redirect.
       (else res))))

 (: ws-connect (string #!optional (list-of ws-extension) (list-of symbol)
		       -> (or ws-connection (struct response))))
 (define (ws-connect uri #!optional (exts '()) (flags '()))
   (let*-values
       (((wsuri) (ws-uri uri))
	((host) (uri-host wsuri))
	((port) (uri-port wsuri))
	((key) (base64-encode (random-bytes (make-string 16))))
	;; open TCP connection to server, send opening handshake
	((i o)
	 (if (eq? 'wss (uri-scheme wsuri))
	     (ssl-connect* hostname: host port: port)
	     (tcp-connect host port)))
	((o*) (send-client-opening-handshake o wsuri key exts flags)))
     ;; read opening handshake
     (read-server-opening-handshake i o key exts)))

 (: send-frame (ws-connection ws-frame -> undefined))
 (define (send-frame conn frame)
   (let ((f (apply-extension-transforms
	     (extensions conn)
	     out-frame-transform frame)))
     (send-frame* conn (frame-opcode f) (frame-payload-data f) (frame-payload-length f)
		  (frame-mask? f) (frame-fin? f) (frame-rsv f))))

 ;; note that send-frame* takes an opcode rather than an optype
 (: send-frame* (ws-connection fixnum u8vector
			       integer boolean boolean fixnum -> undefined))
 (define (send-frame* conn op data len mask fin rsv)
   (let* ((buf (make-u8vector (+ 14 len) 0))
	  (size ((foreign-lambda* size_t ((u8vector u)
					  (byte op) (u8vector data) (unsigned-integer64 len)
					  (bool mask) (bool fin) (byte rsv)
					  (u8vector key))
				  "
unsigned char *u_orig = u;
*(u++) = ((8 * fin + (rsv & 7)) << 4) + (op & 15);
*u = 128 * mask;
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
  for (size_t i = 0; i < len; ++i) *(u++) ^= key[i % 4];
} else {
  memcpy(u, data, len);
  u += len;
}

C_return(u-u_orig);
"
				  )
		 buf op data len mask fin rsv (blob->u8vector/shared (random-bytes (make-blob 4))))))
     (write-u8vector buf (out-port conn) 0 size)))

 (: fragment! (ws-message -> (list-of ws-frame)))
 (: send-message (ws-connection ws-message -> undefined))
 (: send-text-message (ws-connection string -> undefined))
 (: send-binary-message (ws-connection blob -> undefined))

 ;; TODO: fragment long messages?
 (define (fragment! msg)
   (let ((lf (list (make-ws-frame
		    #t 0 (optype->opcode (message-type msg)) #t
		    (message-size msg) (message-data* msg)))))
     ;; if we are recycling msg from elsewhere (e.g. echoeing), note
     ;; that we forget about the fragmentation.
     (message-frames-set! msg lf) lf))

 (define (send-message conn m)
   ;; let any extensions process the message
   (let ((om (apply-extension-transforms
	      (extensions conn)
	      out-message-transform m)))
					;	     (make-ws-message* type '() data))))
     (for-each (lambda (f) (send-frame conn f)) (fragment! om))))

 (define (send-text-message conn data)
   (send-message
    conn
    (make-ws-message 'text (blob->u8vector/shared (string->blob data)))))

 (define (send-binary-message conn data)
   (send-message
    conn
    (make-ws-message 'binary (blob->u8vector/shared data))))

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

 (: interpret-b0 (ws-connection fixnum --> boolean fixnum fixnum))
 (define (interpret-b0 conn b)
   (if (eof-object? b)
       (ws-exn "connection lost"))
   (let* ((fin (< 0 (bitwise-and b 128)))
	  (vrs (valid-rsv conn))
	  (rsv (arithmetic-shift (bitwise-and b 112) -4))
	  (op  (bitwise-and b 15)))
     (if (< vrs (bitwise-ior rsv vrs))
	 (ws-fail 'protocol-error (sprintf "unsupported RSV bits (~A)\n" rsv)))
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
 (: recv-frame (ws-connection -> ws-frame))
 (define (recv-frame conn)
   (let*-values
       (((i) (in-port conn))
	((b0) (read-byte i))
	((fin rsv op) (interpret-b0 conn b0))
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
       (apply-extension-transforms
	(extensions conn)
	in-frame-transform
	f))))

 (: recv-message-loop* (ws-connection (ws-message -> *) integer -> undefined))
 (: recv-message-loop (ws-connection (ws-message -> *) -> undefined))
 (define (recv-message-loop conn handler)
   (condition-case
    (recv-message-loop* conn handler 0)
    (e (websocket fail)
       (print (get-condition-property e 'fail 'message))
       (ws-close conn (get-condition-property e 'fail 'reason)))))

 (define (recv-message-loop* conn handler count)
   (let ((m (recv-message conn)))
     (if (ws-message? m) (begin
			   (handler m)
			   (recv-message-loop* conn handler (+ 1 count))))))


 (: recv-message (ws-connection -> (or false ws-message)))
 (: recv-message* (ws-connection symbol (list-of ws-frame) -> (or false ws-message)))
 (define (recv-message conn)
   ;; receive (& process, if extensions are present) a single message
   (let ((m (recv-message* conn 'none '())))
     (if (ws-message? m)
	 (let ((mt (apply-extension-transforms (extensions conn) in-message-transform m)))
	   ;; validate text message utf-8
	   (if (and (eq? 'text (message-type mt))
		    (not (utf-valid8 (message-data* mt) (message-size mt) 0)))
	       (ws-fail 'protocol-error "text message contains invalid utf-8")
	       mt))
	 #f)))

 (define (recv-message* conn type frames)
   (let* ((f (recv-frame conn))
	  (op (frame-optype f))
	  (data (frame-payload-data f))
	  (len (frame-payload-length f)))
     ;; consume continuation & control frames until a complete message
     ;; can be assembled
     (case op
       ;; text/bianry
       ((text binary)
	(cond
	 ((not (frame-fin? f))
	  (recv-message* conn op (cons f frames)))
	 ((not (eq? 'none type))
	  (ws-fail 'protocol-error "fragments out of order"))
	 (else
	  (frames->message (reverse (cons f frames))))))
       ;; continuation
       ((continuation)
	(cond
	 ((eq? 'none type) (ws-fail 'protocol-error "nothing to continue"))
	 ((frame-fin? f)    (frames->message (reverse (cons f frames))))
	 (else (recv-message* conn type (cons f frames)))))
       ;; ping/pong
       ((ping)
	(send-frame conn (make-ws-frame #t 0 (optype->opcode 'pong) #t len data))
	(recv-message* conn type frames))
       ((pong)
	(recv-message* conn type frames))
       ((connection-close)
	(if (valid-close-frame-payload? data len)
	    (ws-close conn 'normal-closure)
	    (ws-fail 'protocol-error "invalid close frame payload"))))))

 (: ws-close (ws-connection symbol -> undefined))
 (define (ws-close conn reason)
   ;; send close frame
   (send-frame
    conn
    (make-ws-frame #t 0 (optype->opcode 'connection-close) #t
		   2 (reason->close-code reason)))
   ;; finalise extensions
   (for-each
    (lambda (e)
      ((extension-exit e) (extension-params e) conn))
    (extensions conn)))
 )
