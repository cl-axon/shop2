(defdomain ab (
  (:operator (!a1) () () ((foo)))
  (:operator (!a2) () ((bar)) ())
  (:operator (!b1) ((foo)) () ())
  (:operator (!b2) () () ())

  (:method (t1)
    ()
    (:unordered (a) (b)))
 
  (:method (t2)
    ()
    (:ordered (b) (b) (b)))
 
  (:method (a)
	   ()
	   (:ordered (!a1) (!a2)))
	   
  (:method (b)
	   ((foo))
	   (:ordered (!b1) (!b2)))))
