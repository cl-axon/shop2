(defpackage :shop2-asd
    (:use :common-lisp :asdf)
    )
(in-package :shop2-asd)

(defclass shop-tester (system)
     ()
  (:default-initargs :default-component-class 'cl-file-with-defconstants)
  (:documentation "This class allows overriding of particular 
system behaviors."))

(defclass cl-file-with-defconstants ( cl-source-file )
     ()
  (:documentation "Allows us to quash SBCL errors about 
complex defconstants."))

(asdf:defsystem :shop2
    :serial t
    :class shop-tester
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







   
  


