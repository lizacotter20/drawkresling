(defun c:drawkresling ()

	;get relevant parameters from user
	(setq H (getreal "\nEnter deployed height:"))
	(setq H0 (getreal "\nEnter folded height:"))
	(setq n (getint "\nEnter number of polygon edges:"))

	;useful terms to clean up the calculations
	(setq H0sqr (expt H0 2))
	(setq Hsqr (expt H 2))
	(setq plusminus (- (+ 1 Hsqr) H0sqr))
	(setq minusplus (+ (- 1 Hsqr) H0sqr))
	(setq param (/ pi n))

	;check the design constraint
	(if (> (abs (- Hsqr H0sqr)) (expt (cot param) 2))
	    (progn
	       (print "Your parameters have failed to meet the design constraint |H^2 - H0^2| <= cot^2(pi/n)")
	       (print "Please adjust your parameters and try again")
	       (exit) 
	    )
	    (print '())
	)

	;continue getting relevant parameters from user
	(setq b (getreal "\nEnter length of each polygon edge:"))
	(setq p2 (getpoint "\nPick starting point for first polygon edge:"))

	;find the center of the polygon
	(setq apothem (/ b (* 2 (tan param))))
	(setq p0 (list (+ (car p2) (* 0.5 b)) (+ (cadr p2) apothem)))

	;provide some options to the user
	(while
		(initget "Hole Layers")
		(setq ans (getstring "\nIf you would like a circular hole in the top and bottom polygon or the outline and creases to be in different layers (for laser cutting), choose the appropriate option [Hole/Layers]:"))
		(cond
			((= "Hole" ans) (setq r (getreal "\nEnter the radius of the hole"))
				(if (>= r (* (/ b 2) (csc param)))
		 		 	(command "_circle" p0 r)
		  			(print "the radius of the hole cannot be greater or equal to the radius of the polygon")
		  		)
			)
			((= "Layers" ans) (setq r (getreal "\nEnter the radius of the hole"))
				(if (>= r (* (/ b 2) (csc param)))
		 		 	(command "_circle" p0 r)
		  			(print "the radius of the hole cannot be greater or equal to the radius of the polygon")
		  		)
			)
		)
	)



	;have the user choose a cut pattern
	(initget "Mountain Valley Polygon")
	(setq ans (getstring "\nChoose which edges get cut [Mountain/Valley/Polygon]:"))
	(cond
		((= "Mountain" ans)
	)



	;ask if the user wants the outline and creases in different layers
	(setq bad_res T)
	;(setq layers nil)
	(while bad_res
		(setq response (getstring "\nWould you like the outline and creases to be in different layers (for laser cutting)? <Y/n>"))
		(cond 
			((or (= response "Y") (= response "y")) (setq layers T) (setq bad_res nil)) 
			((or (= response "N") (= response "n")) (setq layers nil) (setq bad_res nil))  
			(T (print "Invalid response, please type Y, y, N, or n")) 
		)
	)
	(setq bad_res T)
	(while bad_res
		(setq response (getstring "\nWould you like a circular hole in the top and bottom polygon? <Y/n>"))
		(cond 
			((or (= response "Y") (= response "y")) (setq layers T) (setq bad_res nil)) 
			((or (= response "N") (= response "n")) (setq layers nil) (setq bad_res nil))  
			(T (print "Invalid response, please type Y, y, N, or n")) 
		)
	)

	;the second point is the desired distance from the user selected first point
	(setq p1 (list (+ (car p2) b) (cadr p2))) 

	;do the calculations for the Kresling
	(setq x1 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ plusminus (* minusplus (cos (* 2 param))))))
	(setq x2 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ minusplus (* plusminus (cos (* 2 param))))))
	(setq denom (+ (expt x2 2) 1))
	(setq c (/ (* b (expt (+ (+ (* H0sqr (expt denom 2)) (* (* (expt x2 3) (cot param)) (+ (* x2 (cot param)) 2))) (expt x2 2)) 0.5)) denom))
	(setq a (* b (expt (+ H0sqr (/ (* (expt x2 2) (expt (csc param) 2)) denom)) 0.5)))

	;d is distance in x from p2 to p3
	(setq d (expt (- (expt a 2) (expt c 2)) 0.5))
	;two points for the side panel
	(setq p3 (list (- (car p2) d) (- (cadr p2) c)))
	(setq p4 (list (+ (car p3) b) (cadr p3)))



	;f is distance in x from p2 to p6
	(setq tabwidth (* 0.33333 apothem)) 
	(setq f (abs (/ tabwidth (tan (/ (* param (- n 2)) 2)))))
	;two points for the tab
	(setq p5 (list (+ (car p3) f) (- (cadr p3) tabwidth)))
	(setq p6 (list (- (car p4) f) (- (cadr p4) tabwidth)))

	(if layers
		(progn
			;draw the outline
			(command "_pline" p1 p4 p6 p5 p3 p2 *Cancel*)
			(command "_layer" "_n" "outline" "")
			(command "_layer" "_color" 4 "outline" "")
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" p2 p1 p3 p4 *Cancel*)
			(command "_layer" "_n" "creases" "")
			(command "_layer" "_color" 3 "creases" "")
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		;select and group the panel and tab
			(setq p7 (list (car p3) (cadr p2)))
			(setq p8 (list (car p1) (cadr p6)))
			(setq set1 (ssget "W" p7 p8))
			(command "_.group" "c" "*" "panel and tab" set1 "")
		)
		;draw one side panel and one tab
		(command "_pline" p1 p2 p3 p4 p1 p3 p5 p6 p4 *Cancel*)
	)

	(repeat n
	 (command "rotate" (entlast) "" p0 "C" (/ 360.0 n))
	)
)