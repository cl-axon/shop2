(defpackage :shop2-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop2-asd)

(defclass has-non-eql-constants-mixin ()
     ()
  (:default-initargs :default-component-class 'cl-file-with-defconstants)
  (:documentation "This is a mixin for systems that have defconstant
 values that are not EQL, meaning that SBCL won't like them.")
  )

(defclass shop-system (system has-non-eql-constants-mixin)
     ()
  (:documentation "This system simply adds the 
has-non-eql-constants-mixin.")
  )

(defclass shop-tester (shop-system)
     ()
  (:documentation "This class allows special system behaviors
for systems used to test shop."))

(defmethod operation-done-p 
           ((o test-op)
            (c shop-tester))
  "We need to make sure that operation-done-p doesn't return its
normal value, or a test-op will be run only once."
  (values nil))

(defclass cl-file-with-defconstants ( cl-source-file )
     ()
  (:documentation "Allows us to quash SBCL errors about 
complex defconstants."))

(defconstant +shop-package+ :common-lisp-user
	     "For now, the shop-tester systems need to know in which package
SHOP2 is defined, because SHOP2 does not export any symbols.")


(defsystem :shop2
    :serial t
    :class shop-system
    :version "1.3"
    :in-order-to ((test-op
		   (test-op :shop-logistic
			    :shop-blocks :shop-umt
			    :shop-depots)))
    :components ((:file "shop2")
		 (:file "state-utils")))


;;;; handle SBCL's strict notion of the way DEFCONSTANT should work. [2006/05/16:rpg]
;;;#+sbcl
;;;(defmethod traverse ((op operation) (c shop-tester))
;;;  (handler-bind ((sb-ext:defconstant-uneql
;;;		     #'(lambda (c)
;;;			 (continue c))))
;;;    (call-next-method)))


#+sbcl
(defmethod perform ((op operation) (c cl-file-with-defconstants))
  (handler-bind ((sb-ext:defconstant-uneql
		     #'(lambda (c)
			 (continue c))))
    (call-next-method)))


;;;---------------------------------------------------------------------------
;;; SHOP-UMT domain tester.
;;;---------------------------------------------------------------------------

(defsystem :shop-umt
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :in-order-to ((test-op (load-op :shop-umt)))
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative "examples" "UMT2")) *load-truename*)
    :components ((:file "UMT2")
		 (:file "pfile1" :depends-on ("UMT2"))
		 (:file "pfile2" :depends-on ("UMT2"))
		 ;; interestingly, pfile3 does not seem solvable.
		 ;; Haven't checked to see why [2006/05/10:rpg]
		 (:file "pfile3" :depends-on ("UMT2"))
		 (:file "plans")
		 ))
    
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-umt))))
  (eval `(,(intern "TEST-UMT-SHOP" +shop-package+))))


(defsystem :shop-blocks
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative "examples" "blocks")) *load-truename*)
    :in-order-to ((test-op (load-op :shop-blocks)))
    :components ((:file "block2")
		 (:file "problem100" :depends-on ("block2"))
		 (:file "problem200" :depends-on ("block2"))
		 (:file "problem300" :depends-on ("block2"))
		 (:file "plans" :depends-on ("problem100" "problem200" "problem300"))))
    
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-blocks))))
  (multiple-value-bind (success failed-test-names)
      (eval (list (intern "BW-TESTS" +shop-package+)))
    (if success t
      (progn
	(warn "Failed the following blocks world tests: ~S" failed-test-names)
	;; this seems like The Wrong Thing --- there should be a
	;; specific test-failed error... [2006/05/09:rpg]
	(cerror "Continue and return nil from perfoming test-op."
		(make-condition 'operation-error
		  :component component
		  :operation op))
	nil))))



(defsystem :shop-depots
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative "examples" "depots")) *load-truename*)
    :in-order-to ((test-op (load-op :shop-depots)))
    :components ((:file "depots")
		 (:file "pfile1" :depends-on ("depots"))
		 (:file "pfile2" :depends-on ("depots"))
		 (:file "pfile3" :depends-on ("depots"))
		 (:file "pfile4" :depends-on ("depots"))
		 (:file "pfile5" :depends-on ("depots"))
		 (:file "pfile6" :depends-on ("depots"))
		 (:file "pfile7" :depends-on ("depots"))
		 (:file "pfile8" :depends-on ("depots"))
		 (:file "pfile9" :depends-on ("depots"))
		 (:file "pfile10" :depends-on ("depots"))
		 (:file "pfile11" :depends-on ("depots"))
		 (:file "pfile12" :depends-on ("depots"))
		 (:file "pfile13" :depends-on ("depots"))
		 (:file "pfile14" :depends-on ("depots"))
		 (:file "pfile15" :depends-on ("depots"))
		 (:file "pfile16" :depends-on ("depots"))
		 (:file "pfile17" :depends-on ("depots"))
		 (:file "pfile18" :depends-on ("depots"))
		 (:file "pfile19" :depends-on ("depots"))
		 (:file "pfile20" :depends-on ("depots"))
		 (:file "pfile21" :depends-on ("depots"))
		 (:file "pfile22" :depends-on ("depots"))
		 (:file "plans")))
    

    
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-depots))))
  (eval (list (intern "TEST-SHOP-DEPOTS" +shop-package+))))

(defsystem :shop-logistic
    :class shop2-asd::shop-tester
    :depends-on (:shop2)
    :pathname #.(merge-pathnames (make-pathname :directory '(:relative "examples" "logistic")) *load-truename*)
    :components ((:file "logistic")
		 (:file "Log_ran_problems_15" :depends-on ("logistic"))
		 (:file "Log_ran_problems_20" :depends-on ("logistic"))
		 (:file "Log_ran_problems_25" :depends-on ("logistic"))
		 (:file "Log_ran_problems_30" :depends-on ("logistic"))
		 (:file "Log_ran_problems_35" :depends-on ("logistic"))
		 (:file "Log_ran_problems_40" :depends-on ("logistic"))
		 (:file "Log_ran_problems_45" :depends-on ("logistic"))
		 (:file "Log_ran_problems_50" :depends-on ("logistic"))
		 (:file "Log_ran_problems_55" :depends-on ("logistic"))
		 (:file "Log_ran_problems_60" :depends-on ("logistic"))
		 (:file "plans")
		 )
    :in-order-to ((test-op (load-op :shop-logistic))))
    
;;; needs to be changed to check the results we find.
(defmethod perform :after ((op test-op) (component (eql (find-system :shop-logistic))))
  (eval `(,(intern "TEST-LOGISTICS-PLANS" +shop-package+))))

;;; make sure we don't do this only once...


