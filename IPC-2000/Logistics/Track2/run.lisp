(setf file-list '(
"LOGISTICS-10-0"
"LOGISTICS-10-1"
"LOGISTICS-11-0"
"LOGISTICS-11-1"
"LOGISTICS-12-0"
"LOGISTICS-12-1"
"LOGISTICS-13-0"
"LOGISTICS-13-1"
"LOGISTICS-14-0"
"LOGISTICS-14-1"
"LOGISTICS-15-0"
"LOGISTICS-15-1"
"LOGISTICS-18-0"
"LOGISTICS-18-1"
"LOGISTICS-20-0"
"LOGISTICS-20-1"
"LOGISTICS-25-0"
"LOGISTICS-25-1"
"LOGISTICS-30-0"
"LOGISTICS-30-1"
"LOGISTICS-4-0"
"LOGISTICS-4-1"
"LOGISTICS-4-2"
"LOGISTICS-5-0"
"LOGISTICS-5-1"
"LOGISTICS-5-2"
"LOGISTICS-6-0"
"LOGISTICS-6-1"
"LOGISTICS-6-2"
"LOGISTICS-6-9"
"LOGISTICS-7-0"
"LOGISTICS-7-1"
"LOGISTICS-8-0"
"LOGISTICS-8-1"
"LOGISTICS-9-0"
"LOGISTICS-9-1"
))

(format t "Loading SHOP2 ...~%")
(load "../../../shop2")

(format t "Loading solution-converter ...~%")
(load "../solution-converter")

(format t "Loading logistics ...~%")
(format t "~%")
(load "logistics")
(format t "~%")

(dolist (fname file-list)
  (setf pfilename (concatenate 'string "prob" (string fname) ".lisp"))
  (setf sfilename (concatenate 'string "prob" (string fname) ".soln"))

  (format t "--------------------------------------------------~%")
  (format t "Loading problem file: ~A ...~%" pfilename)
  (load pfilename)
  (format t "~%")

  (format t "Solving problem: ~A ...~%" fname)
  (multiple-value-bind (sol soltime)
                       (find-plans (read-from-string fname) :verbose 0)
    (if sol
      (progn
        (format t "*** Plan found in ~A seconds ***~%" soltime)
        (solution-converter sol soltime "domain.pddl" sfilename nil))
      (format t "*** No Plan ***~%"))
  )
  (format t "~%")
)

