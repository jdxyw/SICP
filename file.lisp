(defun main()
  (let ((in (open "data")))
      (format t "~a~%" (read-line in))
      (close in)))

(defun sample()
  (let ((in (open "data" :if-does-not-exist nil)))
    (when in
      (format t "~a~%" (read-line in))
      (close in))))

(defun sample2()
  (let ((in (open "data")))
    (when in
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))

(defun sample3()
  (let ((in (open "data"))
	(arr (make-sequence 'list 20)))
    (when in
      (read-sequence arr in)
      (close in)
      arr)))

(defun sample2v2(filename)
  (let ((in (open filename)))
    (when in
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))

(defun sample4()
  (let ((out (open "dataout" :direction :output :if-does-not-exist :create)))
    (when out
      (write-line "Hello world" out)
      (write-line "Hello Lisp" out)
      (write-string "Hello Python" out)
      (write-string "Hello Compiler" out)
      (write-char #\r out)
      (close out))
    (sample2v2 "dataout")))

(defun sample5()
  (with-open-file (stream "data")
    (format t "~a~%" (read-line stream))))

(defun sample6()
  (let ((s (make-string-input-stream "1.23")))
    (unwind-protect (read s)
      (close s))))

(defun sample7()
  (with-input-from-string (s "1.23 45 67")
    (read s)))

(defun sample8()
  (with-open-file (stream "data2")
    (when stream
      (loop for line = (read-line stream nil)
	    while line do (with-input-from-string (s line)
			    (loop for num = (read s nil)
				  while num do (print num)))))))
