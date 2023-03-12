(defpackage cl-images
  (:nicknames :myim)
  (:use :cl)
  (:local-nicknames (:im :imago))
  (:export #:make-im
	   #:pos
	   #:make-pos
	   #:make-anim))
(in-package :cl-images)

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

(defun mandelbrot-pixel (x y w h scale xPos yPos iter-max)
  (let ((p (floor (* 255 (mandelbrot (complex (correct x w scale xPos)
					      (correct y h scale yPos))
				     iter-max)))))
    (im:make-color p p p)))

(defstruct (pos (:constructor make-pos (scale x y)))
  scale x y)

(defun make-im (name w h iters pos)
  (let ((image (im:make-rgb-image w h)))
    (dotimes (x w)
      (format t "progress: ~2$%~%" (* 100.0 (/ x w)))
      (dotimes (y h)
	(setf (im:image-pixel image x y)
	      (mandelbrot-pixel x y w h
				(pos-scale pos)
				(pos-x pos)
				(pos-y pos)
				iters))))
    (im:write-png image name)))

;; (0.001 0.4201 -0.2091)

(defun make-anim (name width height iters frames pos-start pos-end)
  (ensure-directories-exist name)
  (format t "making a ~a frame animation~%" frames)
  (make-im (concatenate 'string name "Start.png") width height iters pos-start)
  (make-im (concatenate 'string name "End.png") width height iters pos-end))
