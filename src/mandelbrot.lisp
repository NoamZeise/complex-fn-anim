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
  (round (+ (/ 50 (sqrt (* 0.25 scale))) 80)))

(defun mandelbrot-pixel (x y scale &key (iter-fn #'iter-scale-default))
  (let ((p (floor (* 255 (mandelbrot (complex x y)
				     (funcall iter-fn scale))))))
    (make-colour p p p 255)))



;; 0.001 0.4201 -0.2091

;; 1 -1.5 -0.5
;; 0.001 -1.405 -0.0005

;; 0.001 -1.449 -0.0005

;; little one  - 0.005 -1.45 -0.0025

;; 1 0.2 -0.5
;; 0.0001 0.25045 -0.00005


;;start 3 -2.2 -1.5
;;end 0.054 -1.794 -0.027


;;start 2.95 -2.18 -1.475
;;end 0.0118 -1.6341 -0.0059
