(defparameter data '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(defun loop-sample1()
  (loop
    :for item in data
    :do (print item)))

(defun loop-sample2()
  (loop
    :for item in data by #'cddr
    :collect item))

(defun loop-sample3()
  (loop
    :for item on data
    :collect item))

(defun loop-sample4()
  (loop
    :for item on data
    :collect (reduce #'+ item)))

(defun loop-sample5()
  (loop
    :repeat 50
    :for x = 0 then y
    :for y = 1 then (+ x y)
    :collect y))

(defun loop-sample6()
  (loop
    :repeat 500
    :for x = 0 then y
    :and y = 1 then (+ x y)
    :collect y))

(defun loop-sample7()
  (loop
    :for (a nil) in '((1 2) (3 4) (5 6) (7 8))
    :collect a))

(defparameter *random* (loop :for i from 1 to 1000 :collect (random 1000)))

(defun loop-sample8()
  (loop
    :for i in *random*
    :counting (evenp i) into evens
    :counting (oddp i) into odds
    :summing i into total
    :maximizing i into max
    :minimizing i into min
    :finally (return (list evens odds total max min))))

(defparameter *odds* '(1 3 5 7 9))
(defparameter *evens* '(2 4 6 8 10))

(defun loop-sample9()
  (if (loop :for n in *odds* always (oddp n)) 
      (print "All members are odds")))

(defun loop-sample10()
  (loop
    :for n in data
    :do (when (oddp n) (print "This number is odd"))
    (print n)))

(defun loop-sample11()
  (loop for char across "abc123" thereis (digit-char-p char)))
