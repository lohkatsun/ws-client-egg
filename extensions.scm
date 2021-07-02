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
