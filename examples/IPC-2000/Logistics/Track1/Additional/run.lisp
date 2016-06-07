(setf file-list '(
"logistics-16-0"
"logistics-16-1"
"logistics-17-0"
"logistics-17-1"
"logistics-18-0"
"logistics-18-1"
"logistics-19-0"
"logistics-19-1"
"logistics-20-0"
"logistics-20-1"
"logistics-21-0"
"logistics-21-1"
"logistics-22-0"
"logistics-22-1"
"logistics-23-0"
"logistics-23-1"
"logistics-24-0"
"logistics-24-1"
"logistics-25-0"
"logistics-25-1"
"logistics-26-0"
"logistics-26-1"
"logistics-27-0"
"logistics-27-1"
"logistics-28-0"
"logistics-28-1"
"logistics-29-0"
"logistics-29-1"
"logistics-30-0"
"logistics-30-1"
"logistics-31-0"
"logistics-31-1"
"logistics-32-0"
"logistics-32-1"
"logistics-33-0"
"logistics-33-1"
"logistics-34-0"
"logistics-34-1"
"logistics-35-0"
"logistics-35-1"
"logistics-36-0"
"logistics-36-1"
"logistics-37-0"
"logistics-37-1"
"logistics-38-0"
"logistics-38-1"
"logistics-39-0"
"logistics-39-1"
"logistics-40-0"
"logistics-40-1"
"logistics-41-0"
"logistics-41-1"))


(format t "Loading SHOP2 ...~%")
(load "../../../../shop2")

(format t "Loading solution-converter ...~%")
(load "../../solution-converter")

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

