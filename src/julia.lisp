(in-package :canim)
;; julia set functions -WIP

(defun expt-nothrow (x pow)
  "goes to zero in case of float overflow"
  (handler-case (expt x pow)
    (floating-point-overflow ()
      (format t "float overflow x:~a~%" x)
      0.0d0)))

(defun compexpt (c pow)
  (handler-case
      (+ (expt-nothrow (realpart c) pow) (expt-nothrow (imagpart c) pow))
    (floating-point-overflow ()
      (format t "float addition overflow: ~a~%" c)
      0.0d0)))

(defun julia (z n c R iter-max)
  (/ (dotimes (i iter-max i)
       (if (>= (compexpt z 2.0) (expt R 2.0))
	   (return i))
       (let* ((z-size (expt (compexpt z 2.0) (/ n 2.0)))
	      (inner-trig (* n (atan (imagpart z) (realpart z))))
	      (temp  (+ (* z-size (cos inner-trig)) (realpart c))))
	 (setf z (complex temp (+ (* z-size (sin inner-trig)) (imagpart c))))))
     iter-max))

(defun julia-pixel (x y scale)
  (let* ((n 2) (c (complex 0.285d0 0.01d0)) (R 2.0d0)
	(z (complex x y))
	 (p (floor (* 255 (julia z n c R (iter-scale-default scale))))))
    (make-colour p p p 255)))
