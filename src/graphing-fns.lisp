(in-package :canim)

;;; left-fn(x, y) = right-fn(x, y)
(defun closeness (left-fn right-fn)
  (lambda (x y scale)
    (let* ((cutoff (/ scale 400))
	   (left (funcall left-fn x y))
	   (right (funcall right-fn x y))
	   (dist (abs (- left right))))
      
      (if (> dist cutoff) 1
	  (/ dist cutoff)))))

;;; fn(x, y) = target
(defun const-closeness (fn c)
  (closeness fn (lambda (x y) c)))

;;; fn(x) = y
(defun fn-closeness (fn)
  (closeness
   (lambda (x y) (funcall fn x))
   (lambda (x y) y)))

(defun graph-pixel (fn-list)
  (lambda (x y scale) 
    (let ((intensity 1))
      (loop for fn in fn-list do (setf intensity (* intensity (funcall fn x y scale))))
      (setf intensity (min 255 (floor (* 255 intensity))))
      (make-colour intensity intensity intensity 255))))
