;; websocket extension record
;;(define-type ws-parameters (pair string (list-of (pair string string))))
(define-type ws-extension (struct ws-extension))
(define-type ws-extension-params (list-of (or (pair string string) (pair string true))))
(define-type ws-extension-desc (pair string ws-extension-params))

(: make-ws-extension* ((list-of ws-extension-desc)
		       (ws-frame -> ws-frame) (ws-frame -> ws-frame)
		       (ws-message -> ws-message) (ws-message -> ws-message)
		       (ws-connection -> *) (ws-connection -> *)
		       --> ws-extension))
(: make-ws-extension (string (list-of ws-extension-params)
			     (or false (ws-frame -> ws-frame))
			     (or false (ws-frame -> ws-frame))
			     (or false (ws-message -> ws-message))
			     (or false (ws-message -> ws-message))
			     #!optional
			     (ws-extension-params ws-connection -> *)
			     (ws-extension-params ws-connection -> *)
			     --> ws-extension))
(: extension-desc* (ws-extension --> (list-of ws-extension-desc)))
(: extension-desc*-set! (ws-extension (list ws-extension-desc) -> undefined))
(: extension-desc (ws-extension --> ws-extension-desc))
(: extension-token (ws-extension --> string))
(: extension-params (ws-extension --> ws-extension-params))
(: extension-param-value (string ws-extension-params --> (or boolean string)))

(: in-frame-transform (ws-extension --> (ws-frame -> ws-frame)))
(: out-frame-transform (ws-extension --> (ws-frame -> ws-frame)))
(: in-message-transform (ws-extension --> (ws-message -> ws-message)))
(: out-frame-transform (ws-extension --> (ws-message -> ws-message)))

(: extension-init (ws-extension --> (ws-extension-params ws-connection -> undefined)))
(: extension-exit (ws-extension --> (ws-extension-params ws-connection -> undefined)))

(define-record-type ws-extension
  (make-ws-extension* desc* ift oft imt omt ini exi)
  ws-extension?
  (desc* extension-desc* extension-desc*-set!)
  (ift in-frame-transform)
  (oft out-frame-transform)
  (imt in-message-transform)
  (omt out-message-transform)
  ;; if offer acceptable, initialises state and return parameters.
  ;; if offer unacceptable & do not wish to use extension, return #f.
  (ini extension-init)
  (exi extension-exit))

(define (extension-desc e)
  (let ((d (extension-desc* e)))
    (if (< 1 (length d)) (ws-exn "extension parameters not yet set"))
    (car d)))

(define (extension-token e) (caar (extension-desc* e)))
(define (extension-params e) (cdr (extension-desc e)))

(define (extension-param-value name pm) (alist-ref name pm string-ci=?))


(define-record-printer (ws-extension e out)
  (fprintf out "#<ws-extension ~A>" (extension-desc*->string (extension-desc* e))))

(define (make-ws-extension tk pm ift oft imt omt #!optional ini exi)
  (make-ws-extension*
   (map (lambda (p) (cons tk p)) pm)
   (if ift ift identity)
   (if oft oft identity)
   (if imt imt identity)
   (if omt omt identity)
   (if ini ini (lambda (conn) '()))
   (if exi exi (lambda (conn) '()))))

;; looks through a list of extension descriptions; either assigns the
;; first matching parameters to e and return #t, or return #f.
;;
;; note that this simply accepts the list of parameters following the
;; first occurrence of the extension token; it is up to the
;; application to check in extension-init whether the parameters are
;; rubbish.
(define (extension-in-desc* e edl)
  (let* ((tk (extension-token e))
	 (pm (alist-ref tk edl string-ci=?)))
    (if pm
	(begin (extension-desc*-set! e (list (cons tk pm))) #t)
	#f)))

(: extension-desc->string (ws-extension-desc --> string))
(define (extension-desc->string ed)
  (string-intersperse
   (cons (car ed)
	 (map (lambda (p)
		(if (eq? (cdr p) #t)
		    (car p)
		    (conc (car p) "=" (cdr p))))
	      (cdr ed)))
   ";"))

(: extension-desc*->string ((list-of ws-extension-desc) --> string))
(define (extension-desc*->string edl)
  (string-intersperse (map extension-desc->string edl) ","))

(: string->extension-desc (string --> ws-extension-desc))
(define (string->extension-desc s)
  (let* ((l (string-split s ";"))
	 (tk (car l))
	 (pm (map (lambda (li)
		    ;; TODO: handling quoted-string format arguments
		    ;; by splitting with =\" is an unpleasant hack?
		    (let ((p (string-split li "=\" ")))
		      (if (= 1 (length p))
			  (cons (car p) #t)
			  (cons (car p) (cadr p))))) (cdr l))))
    (cons tk pm)))

(: string->extension-desc* (string --> (list-of ws-extension-desc)))
(define (string->extension-desc* s)
  (map string->extension-desc (string-split s ",")))

(: apply-extension-transforms ((list-of ws-extension) (ws-extension -> *) * -> *))
(define (apply-extension-transforms exts t frame)
  (foldl (lambda (f e) ((t e) f)) frame exts))

;; permessage-deflate extension
(foreign-declare "#include <zlib.h>")

;; we would like to use some functions not exposed by the zlib egg, so
;; we instead interface directly with the C zlib:

(define-foreign-record-type (z-stream "z_stream")
  (constructor: make-z-stream)
  (destructor: free-z-stream)
  (unsigned-integer avail_out avail-out))

;; TODO: allow the client to configure these options
(define-foreign-variable Z_DEFLATED int)
(define-foreign-variable Z_DEFAULT_STRATEGY int)

(define-type z-stream (struct z-stream))
(define-type pm-deflate-state (struct pm-deflate-state))

(: in-z-stream (pm-deflate-state --> z-stream))
(: out-z-stream (pm-deflate-state --> z-stream))
(: temp-buffer (pm-deflate-state --> u8vector))
(: temp-buffer-length (pm-deflate-state --> integer))
(: max-window-bits (pm-deflate-state --> integer))
(: no-contect-takeover (pm-deflate-state --> boolean))

(define-record-type pm-deflate-state
  (make-pm-deflate-state izs ozs buf mwb nct)
  pm-deflate-state?
  (izs in-z-stream)
  (ozs out-z-stream)
  (buf temp-buffer)
  (mwb max-window-bits max-window-bits-set!)
  (nct no-context-takeover no-context-takeover-set!))

(define (temp-buffer-length st) (u8vector-length (temp-buffer st)))

(define (pm-deflate-init* st pm conn)
  ;; negotiate parameters. we support anything the server does so here
  ;; we only check constraints on the client's compression
  (let ((cnct (extension-param-value "client_no_context_takeover" pm))
	(cmwb (extension-param-value "client_max_window_bits" pm)))
    ;;    (printf "~A/~A\n" cnct cmwb) ;; DEBUG
    (if cnct (no-context-takeover-set! st #t))
    (if cmwb
	(let ((b (string->number cmwb)))
	  (if (< b 9)
	      (ws-fail 'policy-violation "client_max_window_bits should be no less than 9")
	      (max-window-bits-set! st b)))))
  ;; accept frames with RSV1 bit set
  (valid-rsv-set-bit! conn 4)
  ;; initialise zlib streams
  (inflate-init (in-z-stream st))
  (deflate-init (out-z-stream st) (max-window-bits st)))

(define (pm-deflate-exit* st pm conn)
  (valid-rsv-unset-bit! conn 4)
  (let ((izs (in-z-stream st))
	(ozs (out-z-stream st)))
    ((foreign-lambda int "inflateEnd" z-stream) izs)
    ((foreign-lambda int "deflateEnd" z-stream) ozs)
    (free-z-stream izs)
    (free-z-stream ozs)))

(: z-stream-init (z-stream -> undefined))
;; this is technically illegal: the only reason it works is because
;; everything zlib does with next_in it defers to the first call of
;; deflate/inflate anyway. this might not still be true in future
;; versions of zlib.
(define z-stream-init (foreign-lambda* void ((z-stream zs))
				       "
zs->zalloc  = Z_NULL;
zs->zfree   = Z_NULL;
zs->opaque  = Z_NULL;
zs->next_in = Z_NULL;"))

(: inflate-init (z-stream -> integer))
(define (inflate-init zs)
  (z-stream-init zs)
  (if (> 0 ((foreign-lambda int "inflateInit2" z-stream int) zs -15))
      (ws-fail
       'invalid-frame-payload-data "zlib inflate initialisation failed")))

(: deflate-init (z-stream integer -> fixnum))
(define (deflate-init zs mwb)
  (z-stream-init zs)
  (if (> 0 ((foreign-lambda int "deflateInit2" z-stream int int int int int)
	    zs 3 Z_DEFLATED (- mwb) 9 Z_DEFAULT_STRATEGY))
      (ws-fail
       'invalid-frame-payload-data "zlib deflate initialisation failed")))

(: inflate-message* (pm-deflate-state ws-message -> ws-message))
(define (inflate-message* st m)
  ;; TODO: check for errors, also handle zero RSV
  ;;(printf "inflate message of size ~A\n" (message-size m))
  (let* ((zs (in-z-stream st))
	 (buf (temp-buffer st))
	 (len (temp-buffer-length st))
	 (ret ((foreign-lambda* int ((z-stream zs)
				     (u8vector trg) (int tlen)
				     (u8vector src) (int slen))
				"
int ret;
zs->next_in  = src; zs->avail_in  = slen;
zs->next_out = trg; zs->avail_out = tlen;

ret = inflate(zs, Z_NO_FLUSH);
if (ret < 0) C_return(ret);

zs->next_in=\"\\x00\\x00\\xff\\xff\";
zs->avail_in=4;

ret = inflate(zs,Z_SYNC_FLUSH);
C_return(ret);
")
	       zs buf len (message-data* m) (message-size m))))
    (cond
     ((not (= 0 ret))
      (ws-fail 'invalid-frame-payload-data (sprintf "zlib inflate error (~A)" ret)))
     ((= 0 (avail-out zs))
      (ws-fail 'message-too-big "message too large for inflate buffer"))
     (else
      (message-data*-set! m (subu8vector buf 0 (- len (avail-out zs))))
	  ;; unset RSV1 for deflated message
	  (frame-rsv-unset-bit! (car (message-frames m)) 4))))
      m)


(: deflate-message* (pm-deflate-state ws-message -> ws-message))
(define (deflate-message* st m)
  (if (= 0 (message-size m))
      ;; zlib complains if there is nothing it can do when deflate()
      ;; is called, so the empty message is a special case
      (message-data*-set! m #u8(2 0 0 0 255 255))
      ;; otherwise, do the compression
      (let* ((zs (out-z-stream st))
	     (buf (temp-buffer st))
	     (len (temp-buffer-length st))
	     (ret ((foreign-lambda* int ((z-stream zs)
					 (u8vector trg) (int tlen)
					 (u8vector src) (int slen)
					 (bool nct))
				    "
zs->next_in  = src; zs->avail_in  = slen;
zs->next_out = trg; zs->avail_out = tlen;
C_return(deflate(zs, (nct ? Z_FULL_FLUSH : Z_SYNC_FLUSH)));
")
		   zs buf len (message-data* m) (message-size m)
		   (no-context-takeover st))))
	(cond
	 ((not (= 0 ret))
	  (ws-fail 'invalid-frame-payload-data (sprintf "zlib deflate error (~A)" ret)))
	 ((= 0 (avail-out zs))
	  (ws-fail 'message-too-big "message too large for deflate buffer"))
	 (else
	  (message-data*-set! m (subu8vector buf 0 (- len (avail-out zs) 4)))))))
  m)


(define (permessage-deflate params)
  (let ((state (make-pm-deflate-state
		(make-z-stream)
		(make-z-stream)
		(make-u8vector (arithmetic-shift 1 20))
		15 #f)))
    (make-ws-extension "permessage-deflate" params
		       #f
		       (lambda (f)
			 (if (or (eq? 'text (frame-optype f))
				 (eq? 'binary (frame-optype f)))
			     (frame-rsv-set! f 4))
			 f)
		       (lambda (m) (inflate-message* state m))
		       (lambda (m) (deflate-message* state m))
		       (lambda (pm conn) (pm-deflate-init* state pm conn))
		       (lambda (pm conn) (pm-deflate-exit* state pm conn)))))
