(in-package :canim)

(defstruct pos
  (scale 1)
  (x 0)
  (y 0))

(defun pos-apply (op pos1 pos2)
  (make-pos :scale (funcall op (pos-scale pos1) (pos-scale pos2))
	    :x (funcall op (pos-x pos1) (pos-x pos2))
	    :y (funcall op (pos-y pos1) (pos-y pos2))))

(defun pos-apply-scalar (op scalar pos)
  (make-pos :scale (funcall op scalar (pos-scale pos))
	    :x (funcall op scalar (pos-x pos))
	    :y (funcall op scalar (pos-y pos))))

(defun pos-print (pos)
  (format t "x: ~a - y: ~a - scale: ~a~%"
	  (pos-x pos)
	  (pos-y pos)
	  (pos-scale pos))
  pos)
