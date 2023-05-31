(defpackage canim
  (:use :cl)
  (:local-nicknames (:im :imago))
  (:export #:make-colour
	   #:make-im
	   #:make-anim

	   #:pos
	   #:make-pos
	   #:pos-scale
	   #:pos-x
	   #:pos-y

	   #:julia-params
	   #:make-julia-params
	   #:julia-pixel
	   #:julia-pixel-dynamic

	   #:mandelbrot-pixel))
