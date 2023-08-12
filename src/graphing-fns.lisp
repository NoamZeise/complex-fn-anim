(in-package :canim)

;;; left-fn(x, y) = right-fn(x, y)
(defun closeness (left-fn right-fn)
  (flet ((calc-cutoff (fn x y delta)
	   (max (abs (- (funcall fn (+ x delta) y)
			(funcall fn (- x delta) y)))
		(abs (- (funcall fn x (+ y delta))
			(funcall fn x (- y delta)))))))
    (lambda (x y scale)
      (let* ((left (funcall left-fn x y))
	     (right (funcall right-fn x y))
	     (dist (abs (- left right)))
	     (delta (let ((test-change (/ scale 100)))
		      (* test-change (max (calc-cutoff left-fn x y test-change)
					  (calc-cutoff right-fn x y test-change)
					  test-change))))) ;; in case of all cutoffs being 0
	(if (> dist delta) 1
	    (/ dist delta))))))

;;; fn(x, y) = target
(defun const-closeness (fn c &key (flipped nil))
  (if flipped
      (closeness (lambda (x y) c) fn)
      (closeness fn (lambda (x y) c))))

;;; fn(x) = y
(defun fn-closeness (fn &key (flipped nil))
  (closeness
   (lambda (x y) (funcall fn (if flipped y x)))
   (lambda (x y) (if flipped x y))))

(defun graph-pixel (fn-list)
  (lambda (x y scale) 
    (let ((intensity 1))
      (loop for fn in fn-list do (setf intensity (* intensity (funcall fn x y scale))))
      (setf intensity (min 255 (floor (* 255 intensity))))
      (make-colour intensity intensity intensity 255))))
