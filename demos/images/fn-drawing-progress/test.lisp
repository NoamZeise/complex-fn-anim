(in-package :canim)

(defun circles (n)
  (let ((fns (list)))
    (dotimes (i n)
      (let ((index i))
	(setf fns (cons (const-closeness
			 (lambda (x y) (+ (expt x 2) (expt y 2))) (/ (1+ index) 10))
			fns))
	(setf fns (cons (fn-closeness (lambda (x) (expt x (+ 1 index))))
			fns))))
    (setf fns (cons (fn-closeness (lambda (x) (+ 1 (/ x 10)))) fns))
    (setf fns (cons (closeness (lambda (x y) (+ (expt x 2)) (expt y 2)) (lambda (x y) x)) fns))
    (setf fns (cons (fn-closeness (lambda (x) (sin x))) fns))
   ;; (setf fns (cons (const-closeness (lambda (x y) 0) 0) fns))
    fns))

(make-im "build/img16.png" 1000 1000
	 (make-pos :scale 8 :x -4 :y -4)
	 :pixel-fn (graph-pixel (circles 3)))
