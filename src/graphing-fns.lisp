;;; These functions help generate graphs of mathematical functions
;;; and animate them.

(in-package :canim)

(defun left=right (left-fn right-fn &key (thickness 1) (accuracy 1))
  "left(x, y) = right(x, y)
Return a function that can be passed to graph-pixel or graph-anim.
takes two functions f, g, and graphs f(x, y) = g(x, y). Given an x and y
value, returns an intensity between 0 and 1, where 0 is maximum intensity.
The thickenss value changes the line thickness.
The accuracy takes an integer from 0 to 2 inclusive. The higher the 
accuracy, the nicer the graph lines, but it will generate slower."
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

(defun fn=c (fn c &key (thickness 1) (accuracy 1))
  "fn(x, y) = c. 
See left=right for more details."
  (left=right fn (lambda (x y) c) :thickness thickness :accuracy accuracy))

(defun fn=y (fn &key (flipped nil) (thickness 1) (accuracy 1))
  "fn(x) = y. 
See left=right for more details."
  (left=right
   (lambda (x y) (funcall fn (if flipped y x))) (lambda (x y) (if flipped x y))
   :thickness thickness :accuracy accuracy))


(defun graph-pixel (fn-list &key (invert nil))
  "This function can be passed to make-im.
Takes a list of functions that take (x, y, scale) and return an 
intensity value between 0-1 (left=right, fn=c, fn=y generate these).
The intensity is combined through multiplication, then a colour is returned."
  (lambda (x y scale) 
    (let ((intensity 1))
      (loop for fn in fn-list do (setf intensity (* intensity (funcall fn x y scale))))
      (setf intensity (min 255 (floor (* 255 (if invert (- 1 intensity) intensity)))))
      (make-colour intensity intensity intensity 255))))

;;; given a function that returns a list of graphing fns
;;; call it each frame with the current progress of the animation
;;; from 0.0-1.0
(defun graph-anim (fn-list-generator &key (invert nil))
  "This function can be passed to make-anim.
Takes a function that, given animation progress between 0-1, 
returns a list of graphing functions as returned by left=right etc."
  #'(lambda (anim-progress)
      (graph-pixel (funcall fn-list-generator anim-progress) :invert invert)))
