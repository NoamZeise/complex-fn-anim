(in-package :canim)
;; julia set functions -WIP

(defun julia (z n c R iter-max)
  (let ((iters (dotimes (i iter-max i)
		 (if (>= (abs z) (expt R 2))
		     (return i))
		 (let* ((z-size (expt (abs z) (/ n 2)))
			(inner-trig (* n (atan (imagpart z) (realpart z))))
			(temp  (+ (* z-size (cos inner-trig)) (realpart c))))
		   (setf z (complex temp (+ (* z-size (sin inner-trig)) (imagpart c))))))))
    (/ iters iter-max)))
    
(defun julia-pixel (x y scale)
  (format t "x:~d y:~d scale:~d~%" x y scale)
  (make-colour 255 255 255 255))
