(prompt "\nType drawkresling to run.....")
 
(defun c:drawkresling ( / dcl_id flag p2 H H0 n b diameter)

	;flag is for discerning whether the dialog was canceled or hidden for starting point selection
	(setq flag 5)

	;load the dialog 
	(setq dcl_id (load_dialog "drawkresling.dcl"))

	;while the flag is not accept or cancel
	(while (> flag 2)
		;make a new dialog
		(if (not (new_dialog "drawkresling" dcl_id))
			(exit)
		)
		
		;set the values of the edit_boxes to their previous values, if there is one
		(if (= Hstrtrad nil)
			(action_tile "H" "(setq Hstrtrad $value)")
			(set_tile "H" Hstrtrad)
		)
		(if (= H0strtrad nil)
			(action_tile "H0" "(setq H0strtrad $value)")
			(set_tile "H0" H0strtrad)
		)
		(if (= nstrtrad nil)
			(action_tile "n" "(setq nstrtrad $value)")
			(set_tile "n" nstrtrad)
		)
		(if (= bstrtrad nil)
			(action_tile "b" "(setq bstrtrad $value)")
			(set_tile "b" bstrtrad)
		)
		(if (= xstrtrad nil)
			(progn
				(action_tile "x" "(setq xstrtrad $value)")
				(setq xstrtrad "0")
			)
			(set_tile "x" xstrtrad)
		)
		(if (= ystrtrad nil)
			(progn
				(action_tile "y" "(setq ystrtrad $value)")
				(setq ystrtrad "0")
			)
			(set_tile "y" ystrtrad)
		)

		;update string values with the values in the boxes, if they've been changed
		(action_tile "H" "(setq Hstrtrad $value)")
		(action_tile "H0" "(setq H0strtrad $value)") 
		(action_tile "n" "(setq nstrtrad $value)")
		(action_tile "b" "(setq bstrtrad $value)")
		(action_tile "x" "(setq xstrtrad $value)")
		(action_tile "y" "(setq ystrtrad $value)") 

		;set the insertion point to what is in the x and y boxes
		(setq p2 (list (distof (get_tile "x")) (distof (get_tile "y"))))


		;remember which radio button was chosen last time
		(cond
			((= crease_type_trad nil) (setq crease_type_trad "m"))
			((= crease_type_trad "m") (set_tile "mountain" "1"))
			((= crease_type_trad "v") (set_tile "valley" "1")) 
			((= crease_type_trad "p") (set_tile "polygon" "1")) 
		)
		;remember which radio button was chosen last time
		(cond
			((= chir_trad nil) (setq chir_trad "cw"))
			((= chir_trad "cw") (set_tile "cw" "1"))
			((= chir_trad "ccw") (set_tile "ccw" "1"))
		)

		;radio buttons
		(action_tile "mountain" "(setq crease_type_trad \"m\")")
		(action_tile "valley" "(setq crease_type_trad \"v\")")
		(action_tile "polygon" "(setq crease_type_trad \"p\")")
		(action_tile "cw" "(setq chir_trad \"cw\")")
		(action_tile "ccw" "(setq chir_trad \"ccw\")")

		;the diameter edit_box is only enabled when the hole toggle is turned on
		;(mode_tile "diameter" 1)
		(if (= holetrad nil)
			(progn
				(action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq holetrad $value)")
				(mode_tile "diameter" 1)
			)
			(progn
				(set_tile "hole" holetrad)
				(mode_tile "diameter" (- 1 (atoi holetrad)))
				(set_tile "diameter" diameterstrtrad)
			)
		)
		(action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq holetrad $value)")
		(action_tile "diameter" "(setq diameterstrtrad $value)") 
		 
		;remember whether the user previously had the layers option turned on
		(if (= layerstrad nil)
			(action_tile "layers" "(setq layerstrad $value)")
			(set_tile "layers" layerstrad)
		)
		(action_tile "layers" "(setq layerstrad $value)")

		;in order for the user to be able to press ok, make sure the design constrtradaints are not violated and that the parameter types are correct
		(action_tile "accept" "(checktypes)")

		;set canceled to true if the dialog was canceled so we dont do unecessary calculations + drawings
		(action_tile "cancel" "(setq canceled T)")

		;flag to hide the dialog box is 5
		(action_tile "select_pt" "(done_dialog 5)")

		;set the flag to whatever start_dialog pulls from done_dialog
		(setq flag (start_dialog))

		;if the select point button was clicked 
		(if (= flag 5)
			;get the point from the user
			(progn
				(setq p2 (getpoint))
				(setq xstrtrad (rtos (car p2)))
				(setq ystrtrad (rtos (cadr p2)))
			)
		)
	)

	(unload_dialog dcl_id)
	
	;if the dialog was canceled, don't draw anything, otherwise call the appropriate routine to do calculations and drawing
	(if canceled
		(setq canceled nil)
		(progn
			;convert string values to reals or ints
			(setq H (distof Hstrtrad))
			(setq H0 (distof H0strtrad))
			(setq n (atoi nstrtrad))
			(setq b (distof bstrtrad))
			(if (= holetrad "1")
				(setq diameter (distof diameterstrtrad))
				(setq diameter 0)
			)
			;get the latest point from the box
			(setq p2 (list (distof xstrtrad) (distof ystrtrad)))
			;call appropriate drawing routine based on crease pattern type
			(cond
				((= crease_type_trad "m") (drawkreslingmountain H H0 n b p2 crease_type_trad chir_trad holetrad diameter layerstrad))
				((= crease_type_trad "v") (drawkreslingvalley H H0 n b p2 crease_type_trad chir_trad holetrad diameter layerstrad)) 
				((= crease_type_trad "p") (drawkreslingpoly H H0 n b p2 crease_type_trad chir_trad holetrad diameter layerstrad))
			)
		)
	)
	(princ)
)