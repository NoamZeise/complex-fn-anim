(in-package :canim)

(defun gr-2 (c)
  (> (+ (expt (realpart c) 2)
	(expt (imagpart c) 2))
     4))

(defun mandelbrot (c iter-max)
  (let* ((z 0)
	 (c (coerce c '(complex double-float)))
	 (iters
	   (dotimes (i iter-max i)
	     (setf z (+ (* z z) c))
	     (if (gr-2 z) (return i)))))
    (/ iters iter-max)))

;; gives decent iter amounts for a consistent detail across scale sizes
(defun iter-scale-default (scale)
  (round (/ 30 (sqrt scale))))

(defun mandelbrot-pixel (x y scale &key (iter-fn #'iter-scale-default))
  (let ((p (floor (* 255 (mandelbrot (complex x y)
				     (funcall iter-fn scale))))))
    (im:make-color p p p)))




;; 0.001 0.4201 -0.2091

;; 1 -1.5 -0.5
;; 0.001 -1.405 -0.0005

;; 0.001 -1.449 -0.0005

;; little one  - 0.005 -1.45 -0.0025

;; 1 0.2 -0.5
;; 0.0001 0.25045 -0.00005
