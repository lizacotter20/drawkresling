(prompt "\nType drawkreslingvis to run.....")
 
(defun c:drawkreslingvis ( / dcl_id)
(print "newWWWWWWW")

;flag is for discerning whether the dialog was canceled or hidden for starting point selection
(setq flag 5)

;load the dialog 
(setq dcl_id (load_dialog "drawkreslingvis.dcl"))

;while the flag is not accept or cancel
(while (> flag 2)
     ;make a new dialog
     (if (not (new_dialog "drawkreslingvis" dcl_id))
          (exit)
     )
     
     ;set the values of the edit_boxes to their previous values, if there is one
     (if (= Hstr nil)
          (action_tile "H" "(setq Hstr $value)")
          (set_tile "H" Hstr)
     )
     (if (= H0str nil)
          (action_tile "H0" "(setq H0str $value)")
          (set_tile "H0" H0str)
     )
     (if (= nstr nil)
          (action_tile "n" "(setq nstr $value)")
          (set_tile "n" nstr)
     )
     (if (= bstr nil)
          (action_tile "b" "(setq bstr $value)")
          (set_tile "b" bstr)
     )
     (if (= xstr nil)
          (action_tile "x" "(setq xstr $value)")
          (set_tile "x" xstr)
     )
     (if (= ystr nil)
          (action_tile "y" "(setq ystr $value)")
          (set_tile "y" ystr)
     )   

     ;update string values with the values in the boxes, if they've been changed
     (action_tile "H" "(setq Hstr $value)")
     (action_tile "H0" "(setq H0str $value)") 
     (action_tile "n" "(setq nstr $value)")
     (action_tile "b" "(setq bstr $value)")
     (action_tile "x" "(setq xstr $value)")
     (action_tile "y" "(setq ystr $value)") 

     ;radio buttons
     (action_tile "mountain" "(setq crease_type \"m\")")
     (action_tile "valley" "(setq crease_type \"v\")")
     (action_tile "polygon" "(setq crease_type \"p\")")

     ;other options
     (action_tile "hole" "(setq hole $value)")
     (action_tile "layers" "(setq layers $value)")
   
     ;in order for the user to be able to press ok, make sure the design constraints are not violated and that the parameter types are correct
     (action_tile "accept" "(checktypes)")
     ;(action_tile "accept" "(done_dialog 1)")

     ;set canceled to true if the dialog was canceled so we dont do unecessary calculations + drawings
     (action_tile "cancel" "(setq canceled T)")
     ;(action_tile "cancel" "(done_dialog 0)")

     ;flag to hide the dialog box is 5
     (action_tile "select_pt" "(done_dialog 5)")

     ;set the flag to whatever start_dialog pulls from done_dialog
     (setq flag (start_dialog))

     ;if the select point button was clicked 
     (if (= flag 5)
          ;get the point from the user
          (progn
               (setq p2 (getpoint))
               (setq xstr (rtos (car p2)))
               (setq ystr (rtos (cadr p2)))
          )
     )
)

(unload_dialog dcl_id)
(print "unloaded")
(print crease_type)
(print (distof xstr))
(print (distof ystr))
(print (distof Hstr))
(print (distof H0str))       
(print (atoi nstr))
(print (distof bstr)) 

(if canceled
     (print '())
     (progn
          ;convert string values to reals or ints
          (setq H (distof Hstr))
          (setq H0 (distof H0str))
          (setq n (atoi nstr))
          (setq b (distof bstr))
          ;useful terms to clean up the calculations
          (setq H0sqr (expt H0 2))
          (setq Hsqr (expt H 2))
          (setq plusminus (- (+ 1 Hsqr) H0sqr))
          (setq minusplus (+ (- 1 Hsqr) H0sqr))
          (setq param (/ pi n))
     )
)

 
(princ)
 
)
 