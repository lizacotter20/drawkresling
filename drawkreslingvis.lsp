(prompt "\nType drawkreslingvis to run.....")
 
(defun c:drawkreslingvis ( / dcl_id)
 
(setq dcl_id (load_dialog "drawkreslingvis.dcl"))
(if (not (new_dialog "drawkreslingvis" dcl_id))
 (exit)
)
(print "heyyyy")
(set_tile "geometry" "Geometry Parameters")
(action_tile "H" "(setq H $value)")
(action_tile "H0" "(setq H0 $value)")
(action_tile "n" "(setq n $value)")
(action_tile "b" "(setq b $value)")

(action_tile "accept" "(checktypes)")
(action_tile "cancel" "(setq canceled T)")
 
(start_dialog)
(unload_dialog dcl_id)
 
(print "heyyyy2")
(print canceled)
(print (distof H))
(print (distof H0))       
(print (atoi n))
(print (distof b)) 

(if canceled
     (print '())
     (progn
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
 