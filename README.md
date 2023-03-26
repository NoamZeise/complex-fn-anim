# canim - complex function animations and images (WIP)

# Usage

Requires a common lisp implimentation (tested with sbcl) and quicklisp.

evaluate the asd file in your repl

```lisp
(load "canim.asd")
```

load the package with quicklisp, which will install any dependancies for your
```lisp
(ql:quickload "canim")
```

Now you can use the library

## creating images

![alone mandelbrot](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/images/alone.png?raw=true)


By default images are created using mandelbrot, supply your own functions with the `:pixel-fn` arg.
Supplied functions must take an x and y value for a point on the complex plane, 
as well as the scale of the image. They return a color
Here is what the default function roughly looks like.

```lisp
(defun mandelbrot-pixel (x y scale)
  (let ((p (floor (* 255 (mandelbrot (complex x y)
				     (iter-fn scale))))))
    (make-colour p p p 255)))
```

To make an image you can call

```
(canim:make-im "my-im.png" 100 100 
	(canim:make-pos :x 0 :y 0.5 :scale 1))
```

## creating an animation

![mandelbrot recursive](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/videos/recursive.gif?raw=true)


To make an animation you can call

```lisp
(canim:make-anim "anim-img-folder/frame" 250 250 50 
	(canim:make-pos :x -1 :y 0.5 :scale 1)
	(canim:make-pos :x -1 :y 0.005 :scale 0.01))
```

Which will output a series of files in the `anim-img-folder` folder with names 
`framexx.png`.

### using a custom pixel function example
![red grad gif](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/videos/redgrad.gif?raw=true)

This was made using

```lisp
(canim:make-anim "build/myanim/" 100 100 100
		 (canim:make-pos :x -1) (canim:make-pos :x 2.0)
			 :pixel-fn #'(lambda (x y scale)
			       (canim:make-color (max (min
						       (floor (* 255
								 (if (> x 1.0) (- 2.0 x) x)))
						       255)
						      0)
						 0 0 255)))
```
Then converted to a gif using imagemagick:
```
$ convert --delay 2 build/myanim/*.png redgrad.gif
```

![mandelbrot gif](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/videos/right-hq.gif?raw=true)

![mandelbrot gif](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/videos/mandelbrot-1.gif?raw=true)

