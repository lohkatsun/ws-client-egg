(import ws (chicken format))

(define uri "ws://localhost:9001")

(define (get-case-count)
  (let ((n 0))
  (let ((c (ws-connect (sprintf "~A/getCaseCount" uri))))
    (set! n (string->number (message-data (recv-message c))))
    (recv-message-loop c (lambda (m) '())))
  n))

(define (run-case n total)
  (if (<= n total)
      (begin
	(printf "case ~A/~A\n" n total)
	(let ((c (ws-connect (sprintf "~A/runCase?case=~A&agent=ws-egg" uri n))))
	  (recv-message-loop
	   c
	   (lambda (m)
	     (send-message c (message-type m) (message-data* m)))))
	(run-case (+ n 1) total))))

(define (update-report)
  (let ((c (ws-connect (sprintf "~A/updateReports?agent=ws-egg" uri))))
    (recv-message-loop c (lambda (m) '()))))

(run-case 1 (get-case-count))
(update-report)
