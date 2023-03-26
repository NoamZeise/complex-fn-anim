(in-package :canim)

(defun make-im (name w h pos &key
			       (show-progress t)
			       (pixel-fn #'mandelbrot-pixel))
  (let ((image (im:make-rgb-image w h)))
    (dotimes (x w)
      (if show-progress
	  (format t "progress: ~2$%~%" (* 100.0 (/ x w))))
      (dotimes (y h)
	(setf (im:image-pixel image x y)
	      (funcall pixel-fn x y w h pos))))
    ;;redirect stdout so write-png doesnt print 
    (with-open-stream (*standard-output* (make-broadcast-stream))
      (im:write-png image name))))

(defun pos-i (current-scale pos1 pos2)
  (let ((scale-factor (/ (- current-scale (pos-scale pos1))
		(- (pos-scale pos2) (pos-scale pos1)))))
    (let ((p (pos-apply #'+
			pos1
			(scalar-pos-apply #'*
					  scale-factor
					  (pos-apply #'- pos2 pos1)))))
      (setf (pos-scale p) current-scale)
      p)))

(defun make-anim (folder width height frames pos-start pos-end
		  &key
		    (show-progress t)
		    (pixel-fn #'mandelbrot-pixel))
  (ensure-directories-exist folder)
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
	(pos-delta (pos-print (scalar-pos-apply #'*
				     (/ 1 (- frames 1))
				     (pos-apply #'- pos-end pos-start)))))
			      
    (dotimes (currentf frames)
      (if show-progress
	  (format t "progress: ~2$%~%" (* 100.0 (/ currentf frames))))
      (make-im (concatenate 'string folder (format nil fmt-str currentf))
	       width height
	       ;; start-scale * scale-change^(current-frame)
	       (pos-print (if (eql (pos-scale pos-delta) 0)
		   (pos-apply #'+ pos-start
			      (scalar-pos-apply #'*
						currentf
						pos-delta))
		   (pos-i (* (expt scale-change currentf)
			     (pos-scale pos-start))
			  pos-start pos-end)))
	       :show-progress nil
	       :pixel-fn pixel-fn))))




;; 0.001 0.4201 -0.2091

;; 1 -1.5 -0.5
;; 0.001 -1.405 -0.0005

;; 0.001 -1.449 -0.0005


;; little one  - 0.005 -1.45 -0.0025
