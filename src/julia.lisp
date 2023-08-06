(in-package :canim)

(defun iter-scale-default (scale)
  "returns an amount of iterations for how detailed the julia set needs to be based on the scale of the image"
  (round (+ (/ 50 (sqrt (* 0.25 scale))) 80)))

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

(defun julia-pixel (n c R)
  "return a function that will give you a julia set pixel function taking (x y scale)."
  #'(lambda (x y scale)
      (let* ((z (complex x y))
	     (p (floor (* 255 (julia z n c R (iter-scale-default scale))))))
	(make-colour p p p 255))))

(defstruct julia-params
  (n 2)
  (c (complex 0 -0.8))
  (R 2))

(defun julia-pixel-dynamic (start end)
  #'(lambda (anim-progress)
      (flet ((interp (fn)
	       (+ (funcall fn start) (* (- (funcall fn end) (funcall fn start)) anim-progress))))
	(julia-pixel (interp #'julia-params-n)
		     (interp #'julia-params-c)
		     (interp #'julia-params-r))))) 

(defun mandelbrot-pixel (x y scale)
  "given a position and scaling value, generate a color to indicate whether this position is in the mandelbrot
set or not."
  (let ((p (floor (* 255 (julia
			  (complex 0 0) 2 (complex x y) 2
			  (iter-scale-default scale))))))
    (make-colour p p p 255)))
