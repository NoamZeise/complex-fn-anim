(in-package :canim)

(defun expt-nothrow (x pow)
  "exponent function that returns zero in the case of float overflow"
  (handler-case (expt x pow)
    (floating-point-overflow ()
      (format t "float overflow x:~a~%" x)
      0.0d0)))

(defun compexpt (c pow)
  "bring the imag and real part of a complex number to a given power and sum.
Returns zero in the case of a float overflow."
  (handler-case
      (+ (expt-nothrow (realpart c) pow) (expt-nothrow (imagpart c) pow))
    (floating-point-overflow ()
      (format t "float addition overflow: ~a~%" c)
      0.0d0)))

(defun julia (z n c R iter-max)
  "compute how much a certain point is in the julia set."
  (/ (dotimes (i iter-max i)
       (if (>= (compexpt z 2.0) (expt R 2.0))
	   (return i))
       (let* ((z-size (expt (compexpt z 2.0) (/ n 2.0)))
	      (inner-trig (* n (atan (imagpart z) (realpart z))))
	      (temp  (+ (* z-size (cos inner-trig)) (realpart c))))
	 (setf z (complex temp (+ (* z-size (sin inner-trig)) (imagpart c))))))
     iter-max))

(defun iter-scale-default (scale)
  "returns an amount of iterations for how detailed the julia set needs to be based on the scale of the image"
  (round (+ (/ 50 (sqrt (* 0.25 scale))) 80)))

(defun julia-pixel (n c R)
  "return a function that will give you a julia set pixel color"
  #'(lambda (x y scale)
      (let* ((z (complex x y))
	     (p (floor (* 255 (julia z n c R (iter-scale-default scale))))))
	(make-colour p p p 255))))

(defstruct julia-params
  (n 2)
  (c (complex 0 -0.8))
  (R 2))

(defun julia-params-interpolate (fn start end progress)
  (+ (funcall fn start) (* (- (funcall fn end) (funcall fn start)) progress)))

(defun julia-pixel-dynamic (start end)
  #'(lambda (anim-progress)
      (julia-pixel (julia-params-interpolate #'julia-params-n start end anim-progress)
		   (julia-params-interpolate #'julia-params-c start end anim-progress)
		   (julia-params-interpolate #'julia-params-r start end anim-progress))))
		    
     

(defun mandelbrot-pixel (x y scale &key (iter-fn #'iter-scale-default))
  "given a position and scaling value, generate a color to indicate whether this position is in the mandelbrot
set or not."
  (let ((p (floor (* 255 (julia
			  (complex 0 0) 2 (complex x y) 2
			  (funcall iter-fn scale))))))
    (make-colour p p p 255)))

;;some mandelbrot values:
;; 0.001 0.4201 -0.2091

;; 1 -1.5 -0.5
;; 0.001 -1.405 -0.0005

;; 0.001 -1.449 -0.0005

;; little one  - 0.005 -1.45 -0.0025

;; 1 0.2 -0.5
;; 0.0001 0.25045 -0.00005


;;start 3 -2.2 -1.5
;;end 0.054 -1.794 -0.027
