(in-package :canim)

;;; left-fn(x, y) = right-fn(x, y)
;;; thickness -> how thick the drawn lines are
;;; accuracy determines how accurate the line thickness is (higher slows down rendering)
;;;     0 -> dont check how fn behaves in the region
;;;     1 -> check horizontal and vertical behavior
;;;     2 -> 1 + check how both diagonals behave
(defun closeness (left-fn right-fn &key (thickness 1) (accuracy 1))
  (flet ((calc-cutoff (fn x y delta) ;; check how fn changes along horz, vert and diag
	   (flet ((cutoff-fn (dx dy)
		    (let ((delta  (/ delta (sqrt (+ (abs dx) (abs dy))))))
		      (flet ((calc (dx dy)
			       (funcall fn (+ x (* dx delta)) (+ y (* dy delta)))))
			(abs (- (calc dx dy) (calc (* dx -1) (* dy -1))))))))
	     (max (if (< accuracy 1) delta 0)
	          (if (>= accuracy 1) (max (cutoff-fn 1 0) (cutoff-fn 0 1)) 0)
		  (if (>= accuracy 2) (max (cutoff-fn 1 1) (cutoff-fn 1 -1)) 0)))))
    (lambda (x y scale)
      (let* ((left (funcall left-fn x y))
	     (right (funcall right-fn x y))
	     (dist (abs (- left right)))
	     (delta (let ((test-change (* (/ scale 500) thickness)))
		      (max (calc-cutoff left-fn x y test-change)
			   (calc-cutoff right-fn x y test-change)
			   (expt test-change 2))))) ;; in case of all cutoffs being 0
	(if (> dist delta) 1
	    (/ dist delta))))))

;;; fn(x, y) = target
(defun const-closeness (fn c &key (thickness 1) (accuracy 1))
  (closeness fn (lambda (x y) c) :thickness thickness :accuracy accuracy))

;;; fn(x) = y
(defun fn-closeness (fn &key (flipped nil) (thickness 1) (accuracy 1))
  (closeness
   (lambda (x y) (funcall fn (if flipped y x)))
   (lambda (x y) (if flipped x y))
   :thickness thickness :accuracy accuracy))

;; take a list of functions returned by the closeness fns
;; and apply each to the given x, y, scale to generate a combined colour
(defun graph-pixel (fn-list &key (invert nil))
  (lambda (x y scale) 
    (let ((intensity 1))
      (loop for fn in fn-list do (setf intensity (* intensity (funcall fn x y scale))))
      (setf intensity (min 255 (floor (* 255 (if invert (- 1 intensity) intensity)))))
      (make-colour intensity intensity intensity 255))))

(defun graph-anim (fn-list-generator &key (invert nil))
  #'(lambda (anim-progress)
      (graph-pixel (funcall fn-list-generator anim-progress) :invert invert)))
