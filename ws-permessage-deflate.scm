;; websocket extension record
(define-type ws-extension (struct ws-extension))
(define-record-type ws-extension
  (make-ws-extension i e ift oft imt omt)
  ws-extension?
  (i extension-init)
  (e extension-exit)
  (ift in-frame-transform)
  (oft out-frame-transform)
  (imt in-message-transform)
  (omt out-message-transform))


(: apply-extension-transforms ((list-of ws-extension)
			       (ws-extension -> (* -> *)) * -> *))
(define (apply-extension-transforms exts t frame)
  (foldl (lambda (f e) ((t e) f)) frame exts))

;; debug extension that simply replaces the first byte of every
;; incoming data frame with 'A' and the final byte of every incoming
;; message with 'e'
(define twiddle
  (make-ws-extension
   (lambda (conn) (print "twiddle init."))
   (lambda (conn) (print "twiddle exit."))
   (lambda (f) (if (and (data-frame? f) (< 0 (frame-payload-length f)))
		   (u8vector-set! (frame-payload-data f) 0 #x41))
	   f)
   identity
   (lambda (m) (let* ((d (message-data* m))
		      (l (u8vector-length d)))
		 (if (< 0 l) (u8vector-set! d (- l 1) #x65)))
	   m)
   identity))

;; what it says on the tin
(define do-nothing
  (make-ws-extension
   (lambda (conn) '())
   (lambda (conn) '())
   identity identity identity identity))
