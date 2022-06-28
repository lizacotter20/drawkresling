(defun c:drawkresling (H H0 n b p2 crease_type hole layers)

(print "still need to fix that weird thing")

;the second point is the desired distance from the user selected first point
(setq p1 (list (+ (car p2) b) (cadr p2))) 

;useful terms to clean up the calculations
(setq H0sqr (expt H0 2))
(setq Hsqr (expt H 2))
(setq plusminus (- (+ 1 Hsqr) H0sqr))
(setq minusplus (+ (- 1 Hsqr) H0sqr))
(setq param (/ pi n))

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

;find the center of the polygon
(setq apothem (/ b (* 2 (tan param))))
(setq p0 (list (+ (car p2) (* 0.5 b)) (+ (cadr p2) apothem)))

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

(repeat (- n 1)
 (command "rotate" (entlast) "" p0 "C" (/ 360.0 n))
)
)