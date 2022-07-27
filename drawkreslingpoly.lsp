(defun drawkreslingpoly (H H0 n b p2 crease_type hole diameter layers / p1 H0sqr Hsqr plusminus minusplus param x1 x2 denom c a g p3 p4 apothem p0 tabwidth j p5 p6 firstt) 

	(defun *error* (msg)
		(if (= msg "Function cancelled")
			(progn
				(print "Function was canceled, exploding groups")
				(command-s "_ungroup" "NA" "panel_and_tab" "")
				(command "_ucs" "W")
			)
			(progn
				(print "Error thrown, exploding groups")
				(command-s "_ungroup" "NA" "panel_and_tab" "")
				(command "_ucs" "W")
			)
		)
	)


	;the second point is the desired distance from the user selected first point
	(setq p1 (list (+ (car p2) b) (cadr p2))) 

	;useful terms to clean up the calculations
	(setq H0scale (/ H0 b))
	(setq Hscale (/ H b))
	(setq H0sqr (expt H0scale 2))
	(setq Hsqr (expt Hscale 2))
	(setq plusminus (- (+ 1 Hsqr) H0sqr))
	(setq minusplus (+ (- 1 Hsqr) H0sqr))
	(setq param (/ pi n))

	;do the calculations for the Kresling
	(setq x1 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ plusminus (* minusplus (cos (* 2 param))))))
	(setq x2 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ minusplus (* plusminus (cos (* 2 param))))))
	(setq denom (+ (expt x2 2) 1))
	(setq c (/ (* b (expt (+ (+ (* H0sqr (expt denom 2)) (* (* (expt x2 3) (cot param)) (+ (* x2 (cot param)) 2))) (expt x2 2)) 0.5)) denom))
	(setq a (* b (expt (+ H0sqr (/ (* (expt x2 2) (expt (csc param) 2)) denom)) 0.5)))

	;g is distance in x from p2 to p3
	(setq g (expt (- (expt a 2) (expt c 2)) 0.5))
	;two points for the side panel
	(setq p3 (list (- (car p2) g) (- (cadr p2) c)))
	(setq p4 (list (+ (car p3) b) (cadr p3)))

	;find the apothem of the polygon
	(setq apothem (/ b (* 2 (tan param))))

	;j is distance in x from p2 to p6
	(setq tabwidth (* 0.33333 apothem)) 
	(setq j (/ tabwidth (tan (/ (* param (- n 2)) 2))))
	;two points for the bottom tab
	(setq p5 (list (+ (car p3) j) (- (cadr p3) tabwidth)))
	(setq p6 (list (- (car p4) j) (- (cadr p4) tabwidth)))
	;two points for the top tab
	(setq p7 (list (+ (car p2) j) (+ (cadr p2) tabwidth)))
	(setq p8 (list (- (car p1) j) (+ (cadr p1) tabwidth)))

	;zoom to drawing area (with lots of margin room)
	(setq bottomleft (list (- (car p3) apothem) (- (cadr p3) (* 4 apothem))))
	(setq topright (list (+ (+ (car p2) (* n b)) tabwidth) (+ (cadr p2) (* 4 apothem))))
	(command "_zoom" bottomleft topright)

	;start drawing
	(if (= layers "1")
		(progn
			;draw the outline
			(command "_pline" p1 p8 p7 p2 *Cancel*)
			(command "_layer" "_n" "outline" "")
			(command "_layer" "_color" 4 "outline" "")
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		(command "_pline" p4 p6 p5 p3 *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" p4 p3 p1 p2 p3 *Cancel*)
			(command "_layer" "_n" "creases" "")
			(command "_layer" "_color" 3 "creases" "")
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		;select and group the panel and tab
			(setq ptList (list p1 p2 p3 p4 p1 p3 p5 p6))
			(setq set1 (ssget "F" ptList))
			(command "_.group" "c" "panel_and_tab" "panel and tab" set1 "")
		)
		;draw one side panel and one tab
		(command "_pline" p1 p8 p7 p2 p3 p5 p6 p4 p3 p1 p2 *Cancel*)
	)

	;translate the tab 
	(setq firstt 1)
	(setq i 0)
	(repeat (- n 1)
		(if (= firstt 1)
			(progn
				(command "move" (entlast) "" p2 p1 "")
				(setq firstt 0)
				(setq i (+ i 1))
			)
			(progn
				(setq pbase (list (+ (car p2) (* i b)) (cadr p2)))
				(setq pdisplace (list (+ (car pbase) b) (cadr pbase)))
				(command "copy" (entlast) "" pbase  pdisplace)
				;if this is our last time through
				(if (= i (- n 2))
					(progn
						;set up new coordinate system for drawing the end tab
						(setq p1t (list (+ (car p1) (* (+ i 1) b)) (cadr p1)))
						(setq p4t (list (+ (car p4) (* (+ i 1) b)) (cadr p4)))
						(setq newp1t (list (- (car p1t) (car p4t)) (- (cadr p1t) (cadr p4t))))
						(command "_ucs" p4t newp1t "")
						(setq distp1p4 (expt (+ (expt (- (cadr p1t) (cadr p4t)) 2) (expt (- (car p1t) (car p4t)) 2)) 0.5))
						(setq newp1tt (list distp1p4 0))
						;two points for the tab
						(setq tab1 (list (- (car newp1tt) j) (* -1 tabwidth)))
						(setq tab4 (list j (* -1 tabwidth)))
						;(command "_ucs" "NA" "S" "endtab")
						;draw the end tab
						(setq origin (list 0 0))
						(command "_pline" newp1tt tab1 tab4 origin *Cancel*)
						(if (= layers "1")
							;add the tab to the outline
							(command "_change" (entlast) "" "_p" "_la" "outline" "")
						)
						(command "_line" newp1tt origin *Cancel*)
						(if (= layers "1")
							;last crease
							(command "_change" (entlast) "" "_p" "_la" "creases" "")
						)
						(command "_ucs" "W")
					)
				)
				(setq i (+ i 1))
			)
		)	
	)

	;make polygon tabs
	(command "_polygon" n "E" p4 p3 *Cancel*)
	(if (= layers "1")
		;add the tab to the outline
		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	)
	(command "_polygon" n "E" p2 p1 *Cancel*)

	(if (= layers "1")
		(progn
			;add the polygon to the outline
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			;delete the crease segments of the polygons
			(command "_break" p3 p4 *Cancel*)
			(command "_break" p2 p1 *Cancel*)
			;draw the last segment of the outline
			(command "_line" p2 p3 *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" p2 p1 p3 p4 *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		(command "_ungroup" "NA" "panel_and_tab")
		)
		;finish the panel
		(command "_pline" p2 p3 p1 *Cancel*)
	)


	(if (= hole "1")
		(progn
			;find the centers of the polygons
			(setq p0 (list (+ (car p2) (* 0.5 b)) (+ (cadr p2) apothem)))
			(setq p00 (list (+ (car p3) (* 0.5 b)) (- (cadr p3) apothem)))
			;draw the circle for the top tab
			(command "_circle" p0 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)
			;draw the circle for the bottom tab
			(command "_circle" p00 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)
		)
	)
)

