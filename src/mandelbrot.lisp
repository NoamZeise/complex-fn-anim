(in-package :canim)

(defun lim (x max) (if (> x max) max x))

(defun gr-2 (c)
  (> (+ (expt (realpart c) 2)
	(expt (imagpart c) 2))
     4))

(defun mandelbrot (c iter-max)
  (let* ((z 0) (c (coerce c '(complex double-float)))
	 (iters
	   (dotimes (i iter-max i)
	     (setf z (+ (* z z) c))
	     (if (gr-2 z) (return i)))))
    (/ iters iter-max)))

(defun correct (pos size scale off)
  (+ (* (/ pos size) scale) off))

;; gives decent iter amounts for a consistent detail across scaling
(defun iter-scale-default (scale)
  (round (/ 30 (sqrt scale))))

(defun mandelbrot-pixel (x y scale &key (iter-fn #'iter-scale-default))
  (let ((p (floor (* 255 (mandelbrot (complex x y)
				     (funcall iter-fn scale))))))
    (im:make-color p p p)))
