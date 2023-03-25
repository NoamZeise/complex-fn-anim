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

(defun pos-apply (op pos1 pos2)
  (make-pos (funcall op (pos-scale pos1) (pos-scale pos2))
	    (funcall op (pos-x pos1) (pos-x pos2))
	    (funcall op (pos-y pos1) (pos-y pos2))))

(defun pos-scalar (op pos scalar)
  (pos-apply op pos (make-pos scalar scalar scalar)))

(defun scale-apply (fn pos)
  (make-pos (funcall fn (pos-scale pos))
	    (pos-x pos)
	    (pos-y pos)))

(defun make-im (name w h iters pos &key (progress t))
  (let ((image (im:make-rgb-image w h)))
    (dotimes (x w)
      (if progress
	  (format t "progress: ~2$%~%" (* 100.0 (/ x w))))
      (dotimes (y h)
	(setf (im:image-pixel image x y)
	      (mandelbrot-pixel x y w h
				(pos-scale pos)
				(pos-x pos)
				(pos-y pos)
				iters))))
    ;;redirect stdout so write-png doesnt print 
    (with-open-stream (*standard-output* (make-broadcast-stream))
      (im:write-png image name))))

;; (0.001 0.4201 -0.2091)

(defun make-anim (name width height iters frames pos-start pos-end)
  (ensure-directories-exist name)
  (format t "making a ~a frame animation~%" frames)
  (let (
	;;ensure output files have consistent number of digits
	 (fmt-str (concatenate 'string "~"
			       (format nil "~d"
				       (length (format nil "~d" frames)))
			       ",'0d.png"))
	 (change (pos-scalar #'/ (pos-apply #'- pos-end pos-start) frames))
	 (scale-change (expt (* (pos-scale pos-start)
				(pos-scale pos-end))
			     (/ 1 (- frames 1)))))
    (dotimes (currentf frames)
      (format t "progress: ~2$%~%" (* 100.0 (/ currentf frames)))
      (make-im (concatenate 'string name (format nil fmt-str currentf))
	       width height iters
	       (pos-apply #'+ pos-start
			  (scale-apply
			   #'(lambda (_) (expt scale-change currentf))
			   (pos-scalar #'* change currentf)))
	       :progress nil))))
	       
  
