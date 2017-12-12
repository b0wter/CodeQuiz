(use srfi-1)

; einfache Lösung: Array bei ~k teilen und wieder zusammensetzen
(define (lshift arr k) (append (drop arr k) (take arr k)))

; elementare Lösung: ein Element nach dem anderen hinten anhängen
(define (lshift2 arr k) (if (eq? 0 k) arr (lshift2 (append (cdr arr) (list (car arr))) (sub1 k))))

; Test
(let* ((k (second (map string->number (string-split (read-line)))))
       (arr       (map string->number (string-split (read-line))))) 
      (begin
		(display (string-intersperse (map number->string (lshift2 arr k)) " " ))
		(newline)
	  )
	 )

(exit)
