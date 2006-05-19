;;;
;;;  We developed this script for use with Allegro Common Lisp running under
;;;  Unix - thus with other Lisps and other platforms, it may not work
;;;  properly.  Here are some examples where it won't  work properly:
;;;
;;; (1) if a platform isn't Posix-compliant, then unix-style pathnames 
;;; won't work. One example of this is Mac OS 9; I suspect Windows might 
;;; also be an example but I'm not sure because I've never used it.  This 
;;; problem, by the way, will also occur with your run.lisp files.
;;;
;;; (2) Some implementations of Common Lisp (MCL, for example) will compile 
;;; files automatically when you load them.  In such systems, compile-file 
;;; is an irrelevant command.
;;;

(format t "Compiling shop2.lisp in Allegro Lisp ... ~%~%")

(compile-file "shop2.lisp")
(compile-file "state-utils.lisp")

