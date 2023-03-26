;; command that generated redgrad.gif


(canim:make-anim "build/myanim/" 100 100 100
		 (canim:make-pos :x -1) (canim:make-pos :x 2.0)
		 :pixel-fn #'(lambda (x y scale)
			       (canim:make-color (max (min
						       (floor (* 255
								 (if (> x 1.0) (- 2.0 x) x)))
						       255)
						      0)
						 0 0 255)))

;;then with imagemagick cli
;; $ convert -delay 2 build/myanim/*.png redgrad.gif
