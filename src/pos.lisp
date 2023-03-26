(in-package :canim)


(defstruct (pos (:constructor make-pos (scale x y)))
  scale x y)

(defun pos-scalar (op pos scalar)
  (pos-apply op pos (make-pos scalar scalar scalar)))

(defun pos-apply (op pos1 pos2)
  (make-pos (funcall op (pos-scale pos1) (pos-scale pos2))
	    (funcall op (pos-x pos1) (pos-x pos2))
	    (funcall op (pos-y pos1) (pos-y pos2))))

(defun scalar-pos-apply (op scalar pos)
  (make-pos (funcall op scalar (pos-scale pos))
	    (funcall op scalar (pos-x pos))
	    (funcall op scalar (pos-y pos))))
  
(defun scale-apply (fn pos)
  (make-pos (funcall fn (pos-scale pos))
	    (pos-x pos)
	    (pos-y pos)))

(defun pos-print (pos)
  (format t "x: ~a - y: ~a - scale: ~a~%"
	  (pos-x pos)
	  (pos-y pos)
	  (pos-scale pos))
  pos)
