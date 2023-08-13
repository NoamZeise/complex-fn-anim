(defun sines ()
  (loop for i from 0 to 8
	collect (let* ((angle (* i (/ PI 4)))
		      (offset angle;(/ PI 4)
			      ))
		  (canim:left=right
		   (lambda (x y) (sin (+ (* y (cos offset))
					 (* x (sin offset))
					 angle)))
		   (lambda (x y) (+ (* x (cos offset))
				    (* y (sin offset) -1)))))))

(canim:make-im "build/test4.png" 1000 1000
	 (canim:make-pos :scale 10 :x -5 :y -5)
	 :pixel-fn (canim:graph-pixel (sines) :invert t))
