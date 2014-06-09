;;;search in google, yahoo, or livedoor
(defun search-engine (q &key (livedoor nil) (yahoo nil))
	(let ((s (make-string-output-stream)))
	(sb-ext:run-program "/usr/bin/python" (cond
																					(yahoo  (list "test.py" q "yahoo"))
																					(livedoor (list "test.py" q "livedoor"))
																					(t (list "test.py" q)))
											:output s)
	(get-output-stream-string s)))

;;;pickup "..." form strings
(defun pick-double-quotation (str)
  (let ((result nil))
	(declare (list result))
  (loop
	for beg = (position #\" str)
	while beg
	for end = (position #\" str :start (1+ beg))
	while end
	do (setf result (cons (subseq str (1+ beg) end) result))
	do (setf str (subseq str (1+ end))))
  (nreverse result)))

;;;http or not
(defun http-p (str)
  (let ((len (length str)))
	(if (> len 4)
	  (string= str "http" :start1 0 :end1 4))))

;;;split 
(defun split (key str)
  (let ((result nil))
	(loop
	  for pos = (position key str)
	  do (setf result (cons (subseq str 0 pos) result))
	  while pos
	  do (setq str (subseq str (1+ pos))))
	(nreverse result)))


(defun input-csv (csv)
	(let ((lst (split #\, csv)))
		lst))

(with-open-file (output "test" )
	(print (read output)))
