(defun numtxt  (str)
 (vl-every '(lambda (x) (<= 48 x 57)) (vl-string->list str))
)