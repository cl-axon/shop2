; This file illustrates the use of the :sort-by keyword in SHOP2.
;  When that keyword is used, alternative possible bindings for a given
;  method are sorted by the value bound to the specified variable.

(defdomain ab (
  (:operator (!a1 ?num) () () ())
  (:operator (!b1 ?num) () () ())

  (:method (a)
	   (foo ?x)
	   ((!a1 ?x)))
	   
  (:method (b)
	   (:sort-by ?x (foo ?x))
	   ((!b1 ?x)))

  (:method (c)
	   (:sort-by (* ?x ?x) #'> (foo ?x))
	   ((!b1 ?x)))))


(defproblem p1 ab
  ((foo 1) (foo 3) (foo -2)) ((a)))

(defproblem p2 ab
  ((foo 1) (foo 3) (foo -2)) ((b)))

(defproblem p3 ab
  ((foo 1) (foo 3) (foo -2)) ((c)))

(find-plans 'p1 :verbose :plans :which :first)
(find-plans 'p1 :verbose :plans :which :all)
(find-plans 'p2 :verbose :plans :which :first)
(find-plans 'p2 :verbose :plans :which :all)
(find-plans 'p3 :verbose :plans :which :first)
(find-plans 'p3 :verbose :plans :which :all)

