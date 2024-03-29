(defun gen-circle-anim (anim-progress)
  (let ((fns (list))
	(circle-count 10)
	(circle-size 1.45))
    (dotimes (i circle-count)
      (let* ((index i) ;; to not take a closure of i in closeness fn
	     (offset (+ (- (* (/ 1 (/ circle-count 2))
			      circle-size index)
			   circle-size)
			(* circle-size anim-progress))))
	(if (> offset 0)
	    (setf fns (cons (canim:fn=c (lambda (x y) (+ (expt x 2) (expt y 2)))
				  (expt offset 2)
				  :thickness 4)
			    fns)))))
    fns))

(canim:make-anim "build/circles/" 100 100 100
		 (canim:make-pos :scale 2 :x -1 :y -1)
		 (canim:make-pos :scale 2 :x -1 :y -1)
		 :pixel-meta-fn (canim:graph-anim #'gen-circle-anim))
