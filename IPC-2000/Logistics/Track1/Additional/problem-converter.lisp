(format t "SHOP 2.0 Problem Converter
Copyright (C) 2002  University of Maryland.
This software is distributed on an \"AS IS\" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied.  This software is distributed under an
MPL/GPL/LGPL triple license.  For details, see the software source file.")

;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;; 
;;; The contents of this file are subject to the Mozilla Public License
;;; Version 1.1 (the "License"); you may not use this file except in
;;; compliance with the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;; License for the specific language governing rights and limitations under
;;; the License.
;;; 
;;; The Original Code is SHOP2 Problem Converter.
;;; 
;;; The Initial Developer of the Original Code is the University of
;;; Maryland. Portions created by the Initial Developer are Copyright (C)
;;; 2000 the Initial Developer. All Rights Reserved.
;;; 
;;; Contributor(s):
;;;    Dana S. Nau
;;;    Yue Cao
;;;    Tsz-Chiu Au
;;;    Okhtay Ilghami
;;;    Ugur Kuter
;;;    Steve Mitchell
;;;    J. William Murdock
;;; 
;;; Alternatively, the contents of this file may be used under the terms of
;;; either of the GNU General Public License Version 2 or later (the "GPL"),
;;; or the GNU Lesser General Public License Version 2.1 or later (the
;;; "LGPL"), in which case the provisions of the GPL or the LGPL are
;;; applicable instead of those above. If you wish to allow use of your
;;; version of this file only under the terms of either the GPL or the LGPL,
;;; and not to allow others to use your version of this file under the terms
;;; of the MPL, indicate your decision by deleting the provisions above and
;;; replace them with the notice and other provisions required by the GPL or
;;; the LGPL. If you do not delete the provisions above, a recipient may use
;;; your version of this file under the terms of any one of the MPL, the GPL
;;; or the LGPL.

;;; -----------------------------------------------------------------------
;;; *** Problem Converter ***
;;;
;;; This is a converter which translates PDDL problems to SHOP2 problems.
;;; The main function is problem-converter, which takes two parameters:
;;;
;;;  (1) the name of the input file containing the PDDL problem
;;;  (2) the name of the output file containing the SHOP2 problem
;;;
;;; Notice that you are likely to modify this program for the domain
;;; you are working on.  Usually, the top-level task list of a domain
;;; is different from that of another domains.  Also the domain name
;;; should be changed too.
;;;
;;; *** USAGE NOTE ***
;;; It may not be a good idea to have all characters capitalized in the
;;; output file.  If you want to avoid that, you should run this program
;;; in Allegro's mlisp rather than alisp.
;;;

;;; -----------------------------------------------------------------------
;;; REVISION LOG
;;;
;;; 2002.4.13 (dan)  This version support typed PDDL file and will
;;;                  change (object - typename) to (typename object).
;;;                  This version also change function (= (property ?variable)
;;;                  ?value) to (property ?variable ?value)
;;;


(defun write-SHOP2-problem (shop2-problem-filename
                            problem-name
                            domain-name
                            objects-list
                            init-list
                            goal-list
                            metric-list)
  (with-open-file (outfile shop2-problem-filename :direction :output
                                                  :if-exists :supersede)
    (format outfile  "(defproblem ~A ~A~%" problem-name domain-name)
    (format outfile "  (~%")

    (format outfile "    ;;;~%")
    (format outfile "    ;;;  Facts~%")
    (format outfile "    ;;;~%")

    (let* ((ol objects-list)
           (stack nil)
           s)
      (do* ()
           ((null ol))
        (setf s (pop ol))
        (if (eql s '-)
            (progn
              (setf s (pop ol))
              (dolist (k stack)
                 (format outfile "    (~A ~A)~%" s k))
              (setf stack nil))
            (push s stack))))

    (format outfile "    ;;;~%")
    (format outfile "    ;;;  Initial states~%")
    (format outfile "    ;;;~%")

    (dolist (s init-list)
      (if (eql (first s) '=)
        (format outfile "    ~A~%" (append (second s) (list (third s))))
        (format outfile "    ~A~%" s)
      )
    )

    (format outfile "  )~%")
    (format outfile "  ;;;~%")
    (format outfile "  ;;;  Goals (task list)~%")
    (format outfile "  ;;;~%")
    (format outfile "  (:unordered ~%")
    (dolist (s (cdr goal-list))
      (format outfile "    ~A~%" (cons ":task" s))
    )
    (format outfile "  )~%")
    (format outfile ")~%")
  )
)

(defun problem-converter (pddl-problem-filename shop2-problem-filename)
  (with-open-file (infile pddl-problem-filename :direction :input)
    (let* ((pddl-problem (read infile))
            problem-name
            domain-name
            objects-list
            init-list
            goal-list
            metric-list)
      ;;;
      ;;; read the PDDL problem
      ;;;
      (dolist (s pddl-problem)
        (when (and (listp s) (eql (first s) 'problem))
          (setf problem-name (second s)))
        (when (and (listp s) (eql (first s) :domain))
          (setf domain-name (second s)))
        (when (and (listp s) (eql (first s) :objects))
          (setf objects-list (cdr s)))
        (when (and (listp s) (eql (first s) :init))
          (setf init-list (cdr s))) 
        (when (and (listp s) (eql (first s) :goal))
          (setf goal-list (second s)))
        (when (and (listp s) (eql (first s) :metric))
          (setf metric-list (second s)))
      )
      
      ;;;
      ;;; Override default values of the domain name
      ;;;                  
      ; (setf domain-name "new-domain-name")
                           
      (write-SHOP2-problem shop2-problem-filename
                           problem-name
                           domain-name
                           objects-list
                           init-list
                           goal-list
                           metric-list)
    )
  )
)

(problem-converter "problogistics-16-0.pddl" "problogistics-16-0.lisp")
(problem-converter "problogistics-16-1.pddl" "problogistics-16-1.lisp")
(problem-converter "problogistics-17-0.pddl" "problogistics-17-0.lisp")
(problem-converter "problogistics-17-1.pddl" "problogistics-17-1.lisp")
(problem-converter "problogistics-18-0.pddl" "problogistics-18-0.lisp")
(problem-converter "problogistics-18-1.pddl" "problogistics-18-1.lisp")
(problem-converter "problogistics-19-0.pddl" "problogistics-19-0.lisp")
(problem-converter "problogistics-19-1.pddl" "problogistics-19-1.lisp")
(problem-converter "problogistics-20-0.pddl" "problogistics-20-0.lisp")
(problem-converter "problogistics-20-1.pddl" "problogistics-20-1.lisp")
(problem-converter "problogistics-21-0.pddl" "problogistics-21-0.lisp")
(problem-converter "problogistics-21-1.pddl" "problogistics-21-1.lisp")
(problem-converter "problogistics-22-0.pddl" "problogistics-22-0.lisp")
(problem-converter "problogistics-22-1.pddl" "problogistics-22-1.lisp")
(problem-converter "problogistics-23-0.pddl" "problogistics-23-0.lisp")
(problem-converter "problogistics-23-1.pddl" "problogistics-23-1.lisp")
(problem-converter "problogistics-24-0.pddl" "problogistics-24-0.lisp")
(problem-converter "problogistics-24-1.pddl" "problogistics-24-1.lisp")
(problem-converter "problogistics-25-0.pddl" "problogistics-25-0.lisp")
(problem-converter "problogistics-25-1.pddl" "problogistics-25-1.lisp")
(problem-converter "problogistics-26-0.pddl" "problogistics-26-0.lisp")
(problem-converter "problogistics-26-1.pddl" "problogistics-26-1.lisp")
(problem-converter "problogistics-27-0.pddl" "problogistics-27-0.lisp")
(problem-converter "problogistics-27-1.pddl" "problogistics-27-1.lisp")
(problem-converter "problogistics-28-0.pddl" "problogistics-28-0.lisp")
(problem-converter "problogistics-28-1.pddl" "problogistics-28-1.lisp")
(problem-converter "problogistics-29-0.pddl" "problogistics-29-0.lisp")
(problem-converter "problogistics-29-1.pddl" "problogistics-29-1.lisp")
(problem-converter "problogistics-30-0.pddl" "problogistics-30-0.lisp")
(problem-converter "problogistics-30-1.pddl" "problogistics-30-1.lisp")
(problem-converter "problogistics-31-0.pddl" "problogistics-31-0.lisp")
(problem-converter "problogistics-31-1.pddl" "problogistics-31-1.lisp")
(problem-converter "problogistics-32-0.pddl" "problogistics-32-0.lisp")
(problem-converter "problogistics-32-1.pddl" "problogistics-32-1.lisp")
(problem-converter "problogistics-33-0.pddl" "problogistics-33-0.lisp")
(problem-converter "problogistics-33-1.pddl" "problogistics-33-1.lisp")
(problem-converter "problogistics-34-0.pddl" "problogistics-34-0.lisp")
(problem-converter "problogistics-34-1.pddl" "problogistics-34-1.lisp")
(problem-converter "problogistics-35-0.pddl" "problogistics-35-0.lisp")
(problem-converter "problogistics-35-1.pddl" "problogistics-35-1.lisp")
(problem-converter "problogistics-36-0.pddl" "problogistics-36-0.lisp")
(problem-converter "problogistics-36-1.pddl" "problogistics-36-1.lisp")
(problem-converter "problogistics-37-0.pddl" "problogistics-37-0.lisp")
(problem-converter "problogistics-37-1.pddl" "problogistics-37-1.lisp")
(problem-converter "problogistics-38-0.pddl" "problogistics-38-0.lisp")
(problem-converter "problogistics-38-1.pddl" "problogistics-38-1.lisp")
(problem-converter "problogistics-39-0.pddl" "problogistics-39-0.lisp")
(problem-converter "problogistics-39-1.pddl" "problogistics-39-1.lisp")
(problem-converter "problogistics-40-0.pddl" "problogistics-40-0.lisp")
(problem-converter "problogistics-40-1.pddl" "problogistics-40-1.lisp")
(problem-converter "problogistics-41-0.pddl" "problogistics-41-0.lisp")
(problem-converter "problogistics-41-1.pddl" "problogistics-41-1.lisp")


