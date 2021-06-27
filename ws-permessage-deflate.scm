;; websocket extension record

(define-record-type ws-extension
  (make-ws-extension* ift oft imt omt op rsv)
  ws-extension?
  (op extension-opcodes)
  (rsv extension-rsv)
  (ift in-frame-transform)
  (oft out-frame-transform)
  (imt in-message-transform)
  (omt out-message-transform))


(: apply-extension-transforms ((list-of (struct ws-extension))
			       ((struct ws-extension) -> ((struct ws-frame) -> (struct ws-frame)))
			       (struct ws-frame) --> (struct ws-frame)))
(define (apply-extension-transforms exts t frame)
  (foldl (lambda (f e) ((t e) f)) frame exts))
