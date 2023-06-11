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
  (ensure-directories-exist name)
  (if (eql (pos-scale pos) 0)
      (error "make-im: supplied pos had a scale of 0"))
  (let ((image (im:make-rgb-image w h)))
    (dotimes (x w)
      (if show-progress (format t "progress: ~2$%~%" (* 100.0 (/ x w))))
      (dotimes (y h)
	(setf (im:image-pixel image x y)
	      (flet ((to-graph (position size pos-fn) ;; used on x and y coords of pixel
		       (pixel-to-graph-space
			position size (pos-scale pos) (funcall pos-fn pos))))
		  (funcall pixel-fn (to-graph x w #'pos-x) (to-graph y h #'pos-y)
			   (pos-scale pos))))))
    ;;redirect stdout so write-png doesnt print 
    (with-open-stream (*standard-output* (make-broadcast-stream))
      (im:write-png image name)))
  (if show-progress (format t "progress: 100.0%~%image saved~%")))

(defun get-scale-progress (current-scale pos-start pos-end)
  (/ (- current-scale (pos-scale pos-start))
     (- (pos-scale pos-end) (pos-scale pos-start))))

(defun correct-pos (current-scale pos1 pos2 progress)
  ;; get percent progress towards final scale and use
  ;; that for calculating the current pos
  (let* ((const-scale (equal (pos-scale pos1) (pos-scale pos2)))
	 (scale-factor (if const-scale progress
			   (get-scale-progress current-scale pos1 pos2))))
    (let ((p (pos-apply #'+
			pos1
			(pos-apply-scalar #'*
					  scale-factor
					  (pos-apply #'- pos2 pos1)))))
      (setf (pos-scale p) (if const-scale (pos-scale pos1) current-scale))
      p)))

(defun make-anim (folder width height frames pos-start pos-end
		  &key
		    (show-progress t)
		    (pixel-meta-fn nil)
		    (pixel-fn #'mandelbrot-pixel))
  "creates a series of pngs labled ii.png for i in 0 to frames. 
The supplied folder will be created if it does not exist. 
By default the pixel-fn used is mandelbrot. 
This function has the args (x y scale) for coordinate points, and will be called 
for each pixel in each animation.
a pixel-meta-fn can be supplied. It is a function that takes in a float for the 
progress of the animation from 0.0 to 1.0, and returns a function that is a 
valid pixel function."
  
  (ensure-directories-exist folder)
  
  (if (or (eql (pos-scale pos-start) 0)
	  (eql (pos-scale pos-end) 0))
      (error "make-anim: one of the supplied positions had a scale of 0"))
  
  (if (< frames 2) (error "tried to make animation with less that 2 frame"))
  
  (format t "making a ~a frame animation~%" frames)
  (let (;;ensure output files have consistent number of digits ie 00-10 vs 0-10
	(file-name-fmt-str (concatenate 'string "~"
			      (format nil "~d" (length (format nil "~d" (- frames 1))))
			      ",'0d.png"))
	;;to calc scale changes so that zoom appears to be smooth across scaling-
	;;this formula gives the property that the relative change in scale
	;;is constant no matter how far zoomed in or out:
	;;start-scale * (scale-end / scale-start)^(current-frame/total-frames)
	;; -- here we precompute what we can and take it ^current-frame each frame
	(scale-change (expt (/ (pos-scale pos-end) (pos-scale pos-start))
			    (/ 1 (1- frames)))))
    (dotimes (currentf frames)
      (let ((progress (/ currentf (1- frames))))
	(if show-progress (format t "progress: ~2$%~%" (* 100.0 (/ currentf frames))))
	(make-im (concatenate 'string folder (format nil file-name-fmt-str currentf))
		 width height
		 (correct-pos (* (expt scale-change currentf) (pos-scale pos-start))
			      pos-start pos-end progress)
		 :show-progress nil
		 :pixel-fn (if pixel-meta-fn
			       (funcall pixel-meta-fn progress)
			       pixel-fn)))))
  (if show-progress (format t "progress: 100.0%~%frames saved~%")))
