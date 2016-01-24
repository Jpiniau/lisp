(ql:quickload :lispbuilder-sdl)
(sb-int:with-float-traps-masked (:invalid :inexact :overflow)

(defvar lifearray (make-array '(100 100)))

(defun init-array (lifearray)
	(loop for i from 0 to (1- (array-dimension lifearray 0)) do
  		(loop for j from 0 to (1- (array-dimension lifearray 1)) do
			(setf (aref lifearray i j) 0))))

(defun count-neighborhood (i j lifearray)
	(let ((next-i (if (= i (1- (array-dimension lifearray 0))) 0 (1+ i)))
		(prev-i (if (= i 0) (1- (array-dimension lifearray 0)) (1- i)))
			(next-j (if (= j (1- (array-dimension lifearray 1))) 0 (1+ j)))
				(prev-j (if (= j 0) (1- (array-dimension lifearray 1)) (1- j))))
	(+ (aref lifearray prev-i prev-j) (aref lifearray prev-i j) (aref lifearray prev-i next-j)
		(aref lifearray i prev-j) (aref lifearray i next-j)
		(aref lifearray next-i prev-j) (aref lifearray next-i j) (aref lifearray next-i next-j))))

(defun next-life (lifearray)
	(let ((next-lifearray (copy-array  lifearray)))
		(loop for i from 0 to (1- (array-dimension lifearray 0)) do
			(loop for j from 0 to (1- (array-dimension lifearray 1)) do
				(cond ((and (zerop (aref lifearray i j)) ; birth
					   (= (count-neighborhood i j lifearray) 3))
					   (setf (aref next-lifearray i j) 1))
					 ((and (= (aref lifearray i j) 1)   ; die
					 (or (<= (count-neighborhood i j lifearray) 1)
					(>= (count-neighborhood i j lifearray) 4)))
					(setf (aref next-lifearray i j) 0)))))
		      next-lifearray))
(defun main-loop()
	(when (sdl:mouse-right-p)
	  ;(setf (aref lifearray (/ (sdl:mouse-x) 500) (/ (sdl:mouse-y) 500) 1)))
	  (print ( / (sdl:mouse-x) 5)))
	(when (sdl:mouse-left-p)
	  (print "left"))
)

(defun test()
	(sdl:with-init ()
		(sdl:window 500 500
			:resizable t
			:double-buffer t
			:title-caption "Tutorial 1"
			:icon-caption "Tutorial 1")
		(init-array lifearray)
		(setf (sdl:frame-rate) 10)
		(sdl:with-events ()
			(:quit-event () t)
			(:idle ()
				(main-loop)
				(setf lifearray (next-life lifearray))
					(loop for i from 0 to (1- (array-dimension lifearray 0)) do
						(loop for j from 0 to (1- (array-dimension lifearray 1)) do
							(if (= (aref lifearray i j) 0)
		    					(sdl:draw-box (sdl:rectangle :x (* i 5) :y (* j 5) :w 4 :h 4)
									:color sdl:*green*)
			 		  			(sdl:draw-box (sdl:rectangle :x (* i 5) :y (* j 5) :w 4 :h 4)
									:color sdl:*red*))))
			;	(main-loop)
				(sdl:update-display))
				(exit))
		;(clean-up)
))
(test)
(quit))
