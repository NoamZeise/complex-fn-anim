(defun grid (angle)
  (canim:left=right
   (lambda (x y) (cos (* 10 (+ (* x (cos angle))
			       (* y (sin angle) -1)))))
   (lambda (x y) (sin (* 10 (+ (* y (cos angle))
			       (* x (sin angle))))))))

(canim:make-im "build/rot-grid.png" 1000 1000
	       (canim:make-pos :scale 10 :x -5 :y -5)
	       :pixel-fn (canim:graph-pixel
			  (loop for i upto 10 collect (let ((index i)) (grid (* index (/ PI 5)))))))
