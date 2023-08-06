# canim - complex function animations and images (WIP)

# Usage

The executable will read a single lisp form, which you can use to generate images or 
animations.

To build, requires a common lisp implimentation (tested with sbcl) and quicklisp.
run make to create an exported binary.

### from a repl

Evaluate the asd file in your repl

```lisp
(load "canim.asd")
```

Load the package with quicklisp, which will install any dependancies for your
```lisp
(ql:quickload "canim")
```

Now you can use the library functions

## creating images

![alone mandelbrot](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/images/alone.png?raw=true)


By default images are created using mandelbrot, julia set functions are also supplied.
Supply your own functions with the `:pixel-fn` arg.
Supplied functions must take an x and y value for a point on the complex plane, 
as well as the scale of the image. They return a color.

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

### Using a Pixel Meta Function

Animations also support passing a `pixel-meta-fn`. This allows you to change the pixel function per frame. The meta function must take in a number from `0-1` representing the progress of the animation, and return a valid pixel function.


For example, included in the package is a function called `julia-pixel-dynamic` 
which you can supply as a meta function.
You give this function start and end parameters for the julia set, pass the result as 
the meta function, and the animation will interpolate between them.


Here is an example of using the julia pixel meta function.


```lisp
(canim:make-anim "../build/" 1000 1000 20
		 (canim:make-pos :x -1.5 :y -1.5 :scale 3)
		 (canim:make-pos :x -1.5 :y -1.5 :scale 3)
		 :pixel-meta-fn
		 (canim:julia-pixel-dynamic
		  (canim:make-julia-params :c (complex 0 0.65))
		  (canim:make-julia-params :c (complex 0 0.635))))
```

Which produces the following animation:

![julia set animation](https://github.com/NoamZeise/complex-fn-anim/blob/master/demos/videos/julia-transition.gif?raw=true)
