(defun c:drawkreslingv ( / dcl_id)

	(setq dcl_id (load_dialog "drawkreslingv.dcl" dcl_id))
		(if (not (new_dialog "drawkreslingv" dcl_id))
			(exit)
		)
	;(set_tile "H" "Enter deployed height:")
	(action_tile "H" "(setq H $value)")
	(action_tile "H0" "(setq H0 $value)")
	(action_tile "n" "(setq n $value)")
	(action_tile "b" "(setq b $value)")
	;(action_tile "p0" "(setq p0 $value)")

	(start_dialog)
	(unload_dialog dcl_id)

)