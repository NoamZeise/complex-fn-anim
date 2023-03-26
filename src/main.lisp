(in-package :canim)

(defun pixel-to-graph-space (component im-dim scale offset)
  (+ (* (/ (+ component
	      0.5)
	   im-dim)
	scale)
     offset))

(defun make-im (name w h pos &key
			       (show-progress t)
			       (pixel-fn #'mandelbrot-pixel))
  "create a png with name of a certain size at pos. 
Position is from bottom left of the image.
pixel-fn must take arguments (x y scale),
where x and y are the coordinates of the point
and scale is the scale of the image 
(for making accuracy adjustments based on scale)."
  (if (eql (pos-scale pos) 0)
      (error "make-im: supplied pos had a scale of 0"))
  (let ((image (im:make-rgb-image w h)))
    (dotimes (x w)
      (if show-progress
	  (format t "progress: ~2$%~%" (* 100.0 (/ x w))))
      (dotimes (y h)
	(setf (im:image-pixel image x y)
	      (funcall pixel-fn
		       ;; x
		       (pixel-to-graph-space x w (pos-scale pos) (pos-x pos))
		       ;; y
		       (pixel-to-graph-space y h (pos-scale pos) (pos-y pos))
		       (pos-scale pos)))))
    ;;redirect stdout so write-png doesnt print 
    (with-open-stream (*standard-output* (make-broadcast-stream))
      (im:write-png image name)))
  (if show-progress
      (format t "progress: 100.0%~%image saved~%")))


(defun correct-pos-with-scale (current-scale pos1 pos2)
  ;; get percent progress towards final scale and use
  ;; that for calculating the current pos
  (let ((scale-factor (/ (- current-scale (pos-scale pos1))
		(- (pos-scale pos2) (pos-scale pos1)))))
    (let ((p (pos-apply #'+
			pos1
			(pos-apply-scalar #'*
					  scale-factor
					  (pos-apply #'- pos2 pos1)))))
      (setf (pos-scale p) current-scale)
      p)))

(defun make-anim (folder width height frames pos-start pos-end
		  &key
		    (show-progress t)
		    (pixel-fn #'mandelbrot-pixel))
  "creates a series of pngs labled iii.png for i in 0 to frames. 
The supplied folder will be created if it does not exist. 
by default the pixel-fn used is mandelbrot. 
This function has the args (x y scale) for coordinate points, and will be called 
for each pixel in each animation."
  (ensure-directories-exist folder)
  (if (or (eql (pos-scale pos-start) 0)
	  (eql (pos-scale pos-end) 0))
      (error "make-anim: one of the supplied positions had a scale of 0"))
  (format t "making a ~a frame animation~%" frames)
  (let (
	;;ensure output files have consistent number of digits
	(fmt-str (concatenate 'string "~"
			      (format nil "~d"
				      (length (format nil "~d" (- frames 1))))
			      ",'0d.png"))

	;;to calc scale changes so that zoom appears to be smooth across scaling-
	;;this formula gives the property that the relative change in scale
	;;is constant no matter how far zoomed in or out:
	;; start-scale * (scale-end / scale-start)^(current-frame/total-frames)
	
	;;here we precompute what we can and take it ^current-frame each frame
	(scale-change (expt (/ (pos-scale pos-end) (pos-scale pos-start))
			    (/ 1 (- frames 1))))
	(pos-delta (pos-apply-scalar #'*
				     (/ 1 (- frames 1))
				     (pos-apply #'- pos-end pos-start))))
    (dotimes (currentf frames)
      (if show-progress
	  (format t "progress: ~2$%~%" (* 100.0 (/ currentf frames))))
      (make-im (concatenate 'string folder (format nil fmt-str currentf))
	       width height
	       ;; start-scale * scale-change^(current-frame)
	       (if (eql (pos-scale pos-delta) 0)
		   (pos-apply #'+ pos-start
			      (pos-apply-scalar #'*
						currentf
						pos-delta))
		   (correct-pos-with-scale (* (expt scale-change currentf)
			     (pos-scale pos-start))
			  pos-start pos-end))
	       :show-progress nil
	       :pixel-fn pixel-fn)))
  (if show-progress
      (format t "progress: 100.0%~%animation saved~%")))
