;; websocket extension record
(define-type ws-extension (struct ws-extension))
(define-type ws-parameters (pair string (list-of (pair string string))))

(define-record-type ws-extension
  (make-ws-extension* pm ift oft imt omt of ac ini exi)
  ws-extension?
  (pm parameters parameters-set!)
  (ift in-frame-transform)
  (oft out-frame-transform)
  (imt in-message-transform)
  (omt out-message-transform)
  (of offer-parameters)
  ;; if offer acceptable, initialises state and return parameters.
  ;; if offer unacceptable & do not wish to use extension, return #f.
  (ac accept-parameters)
  (ini extension-init)
  (exi extension-exit))

(define-record-printer (ws-extension e out)
  (fprintf out "#<ws-extension ~A>" (parameters e)))

(define (make-ws-extension pm ift oft imt omt #!optional of ac ini exi)
  (let ((tk (car pm)))
    (make-ws-extension* pm ift oft imt omt
			;; by default, offer current state with no fallbacks
			(if of of (lambda () pm))
			;; by default, accept server offer & update
			;; parameters to match
			(if ac ac (lambda (h) (extract-parameters tk h)))
			(if ini ini (lambda (conn) #f))
			(if exi exi (lambda (conn) #f)))))

;; TODO: these assume the arguments are well-formed and will die
;; terribly of they aren't
(: extract-parameters (string (list-of ws-parameters) --> ws-parameters))
(define (extract-parameters tk pl)
  (cond
   ((eq? '() pl) #f)
   ((equal? (caar pl) tk) (cdar pl))
   (else (extract-parameters tk (cdr pl)))))

(: parameters->string (ws-parameters --> string))
(define (parameters->string pm)
  (string-intersperse
   (cons (car pm)
	 (map (lambda (p)
		(if (cdr p)
		    (sprintf "~A=~A" (car p) (cdr p))
		    (car p)))
	      (cdr pm)))
   ";"))

(: string->parameters (string --> ws-parameters))
(define (string->parameters s)
  (let* ((pm (string-split s "; "))
	 (tk (car pm))
	 (pl (map (lambda (p) (string-split p "=")) (cdr pm))))
    (cons tk (map (lambda (p) (cons (car p) (if (eq? '() (cdr p)) #f (cadr p)))) pl))))    

(: parameter-list->string ((list-of ws-parameters) --> string))
(define (parameter-list->string l)
  (string-intersperse (map parameters->string l) ","))

(: string->parameter-list (string --> (list-of ws-parameters)))
(define (string->parameter-list s)
  (map string->parameters (string-split s ",")))

(: apply-extension-transforms ((list-of ws-extension)
			       (ws-extension -> (* -> *)) * -> *))
(define (apply-extension-transforms exts t frame)
  (foldl (lambda (f e) ((t e) f)) frame exts))

;; for testing: extensions that swap first two bytes of a message
(define (swaptwo m)
  (if (> (message-size m) 1)
      (let* ((d (message-data* m))
	     (b0 (u8vector-ref d 0)))
	(u8vector-set! d 0 (u8vector-ref d 1))
	(u8vector-set! d 1 b0)))
  m)
  
(define swapin
  (make-ws-extension '("swapin") identity identity swaptwo identity))
(define swapout
  (make-ws-extension '("swapout") identity identity identity swaptwo))

(define permessage-deflate
  (make-ws-extension '("permessage-deflate")
		     identity
		     identity
		     identity
		     identity
		     #f #f
		     (lambda (conn) (print "permessage deflate init"))
		     (lambda (conn) (print "permessage deflate exit"))))
