(canim:make-anim "../build/" 1000 1000 20
		 (canim:make-pos :x -1.5 :y -1.5 :scale 3)
		 (canim:make-pos :x -1.5 :y -1.5 :scale 3)
		 :pixel-meta-fn
		 (canim:julia-pixel-dynamic
		  (canim:make-julia-params :c (complex 0 0.65))
		  (canim:make-julia-params :c (complex 0 0.635))))
