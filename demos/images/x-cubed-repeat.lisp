(canim:make-im
 "build/x-cubed-repeat.png" 1000 1000
 (canim:make-pos :scale 10 :x -5 :y -5)
 :pixel-fn
 (canim:graph-pixel ((let ((num 20) (exponent 3))
		       (nconc (loop
				for i upto num
				collect
				(let ((index i))
				  (canim:left=right (lambda (x y)
						      (expt (* x (/ index (/ num 2))) exponent))
						    (lambda (x y) y))
				  ))
			      (loop
				for i upto num
				collect
				(let ((index i))
				  (canim:left=right (lambda (x y)
						      (expt (* x (/ index (/ num 2)) -1) exponent))
						    (lambda (x y) y))
				  )))))))
