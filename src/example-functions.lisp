(in-package :canim)

(defun grad (x y scale)
  (make-colour (floor (* 255 x)) (floor (* 255 y)) 0 255))

(defun fn-closeness (fn)
  (lambda (x y scale)
    (let* ((cutoff (/ scale 400))
	   (dist (abs (- (funcall fn x) y))))
	 (if (> dist cutoff) 0
	     (- 1 (/ dist cutoff))))))

(defun param-closeness (fn target)
  (lambda (x y scale)
    (let* ((cutoff (/ scale 400))
	   (dist (abs (- target
			 (funcall fn x y)))))
      (if (> dist cutoff) 0
	  (- 1 (/ dist cutoff))))))

(defun fn-colour
    (&key (fn (fn-closeness #'(lambda (x) (expt x 2)))))
  #'(lambda (x y scale) 
      (let* ((ratio (funcall fn x y scale))
	     (colour (- 255 (floor (* 255 ratio)))))
	(make-colour colour colour colour 255))))

(make-im "../build/img6.png" 1000 1000
	 (make-pos :scale 4 :x -2 :y -0.1)
	 :pixel-fn (fn-colour :fn (param-closeness #'(lambda (x y) (- y (expt x 2))) 0)))
