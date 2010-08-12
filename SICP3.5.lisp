(defun memo-proc(proc)
  (let ((already-run nil)
	(result nil))
    (lambda ()
      (if (not already-run)
	(progn
	  (setf result (funcall proc))
	  (setf already-run t)
	  result)
	result))))

(defun force (delayed-object)
  (funcall delayed-object))

(defun stream-car(stream)
  (car stream))

(defun stream-cdr(stream)
  (force (cdr stream)))

(defun stream-null(stream)
  (null stream))

(defmacro cons-stream(a b)
  `(cons ,a (delay ,b)))

(defmacro delay(expr)
  `(memo-proc (lambda () ,expr)))

;(defun stream-ref (stream nth)
;  (if (= nth 0)
;      (stream-car stream)
;      (stream-ref (stream-cdr stream) (- nth 1))))

(defun stream-ref(stream nth)
  (loop
    :for i :from nth :downto 0
    :for c = stream :then (stream-cdr c)
    :finally (return (stream-car c))))

(defun stream-enumerate-interval (low high)
  (if (> low high)
      '()
      (cons-stream
	low
	(stream-enumerate-interval (+ low 1) high))))

(defun stream-filter (pre stream)
  (cond ((stream-null stream) '())
	((funcall pre (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pre (stream-cdr stream))))
	(t (stream-filter pre (stream-cdr stream)))))

(defun stream-map(proc &rest argstreams)
  (if (stream-null argstreams)
      '()
      (cons-stream
	(apply proc (mapcar #'stream-car argstreams))
	(apply #'stream-map 
	       (cons proc (mapcar #'stream-cdr argstreams))))))
;This is the excise 3.51
(defun show(x)
  (print x)
  x)

(defvar x (stream-map #'show (stream-enumerate-interval 0 10)))
;This is the excise 3.52
(defvar sum 0)
(defun accum(x)
  (setf sum (+ x sum))
  sum)

(defvar seq (stream-map #'accum (stream-enumerate-interval 1 20)))

(defun fibgen(a b)
  (cons-stream a (fibgen b (+ a b))))

(defvar fibs (fibgen 0 1))

(defun divisible(x y)
  (= (rem x y) 0))

(defun integers-starting-from(n)
  (cons-stream n 
	       (integers-starting-from (+ n 1))))

(defun sieve(stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter (lambda(x)(not (divisible x (stream-car stream))))
			  (stream-cdr stream)))))

