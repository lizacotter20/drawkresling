(defun c:drawkresling4 ()

;get relevant parameters from user
(setq H (getreal "\nEnter deployed height:"))
(setq H0 (getreal "\nEnter folded height:"))
(setq n (getint "\nEnter number of polygon edges:"))
(setq b (getreal "\nEnter length of each polygon edge:"))
(setq p2 (getpoint "\nPick starting point for first polygon edge:"))

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

;find the center of the polygon
(setq apothem (/ b (* 2 (tan param))))
(setq p0 (list (+ (car p2) (* 0.5 b)) (+ (cadr p2) apothem)))

;f is distance in x from p2 to p6
(setq tabwidth (* 0.33333 apothem)) 
(setq f (abs (/ tabwidth (tan (/ (* param (- n 2)) 2)))))
;two points for the tab
(setq p5 (list (+ (car p3) f) (- (cadr p3) tabwidth)))
(setq p6 (list (- (car p4) f) (- (cadr p4) tabwidth)))

;draw one side panel and one tab
(command "_line" p1 p2 p3 p4 p1 p3 p5 p6 p4 *Cancel*)

;select and group the panel and tab
(setq p7 (list (car p3) (cadr p2)))
(setq p8 (list (car p1) (cadr p6)))
(setq set1 (ssget "W" p7 p8))
(command "_.group" "c" "*" "panel and tab" set1 "")

;rotate the group around p0 n times
(repeat n
 (command "rotate" (entlast) "" p0 "C" (/ 360 n))
)
)