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

;;;csv->list
(defun input-csv (csv)
	(let ((lst (split #\, csv)))
		lst))

(defun include (key str)
	(let ((flag nil)
				(len1 (length str))
				(len2 (length key)))
		(loop
			for i from 0 to (- len1 len2)
			do (if (string= key str :start2 i :end2 (+ i len2))
					 (setq flag t))
			until flag)
		flag))

(defun pick-sites (result)
	(remove-if #'null
	(mapcar 
		#'(lambda (item)
							(if (and (http-p item) 
											 (not (some 
															#'(lambda (key)
																	(include key item))
															'("livedoor" "linecorp" "adsense"))))
								item))
					(remove-duplicates (pick-double-quotation result) :test #'equal))))

(with-open-file (output "test" )
	(let ((data (read output)))
		(print (pick-sites data))))
