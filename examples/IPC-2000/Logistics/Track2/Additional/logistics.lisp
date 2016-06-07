;;;
;;;  AIPS 2000 competition's Logistic domain in SHOP2 syntax  (Translated by Tsz-Chiu Au)
;;;

;;;
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
;;; The Original Code is logistics.lisp in the SHOP2 software distribution.
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
;;;


(defdomain logistics (

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:operator (!load-truck ?pkg ?truck ?loc)
             ((at ?truck ?loc)
              (at ?pkg ?loc))
             ((at ?pkg ?loc)
              (:protection (at ?truck ?loc)))
             ((in ?pkg ?truck)))

  (:operator (!unload-truck ?pkg ?truck ?loc)
             ((at ?truck ?loc)
              (in ?pkg ?truck))
             ((in ?pkg ?truck)
              (:protection (at ?truck ?loc)))
             ((at ?pkg ?loc)))

  (:operator (!load-airplane ?pkg ?airplane ?loc)
             ((at ?pkg ?loc)
              (at ?airplane ?loc))
             ((at ?pkg ?loc)
              (:protection (at ?airplane ?loc)))
             ((in ?pkg ?airplane)))

  (:operator (!unload-airplane ?pkg ?airplane ?loc)
             ((in ?pkg ?airplane)
              (at ?airplane ?loc))
             ((in ?pkg ?airplane)
              (:protection (at ?airplane ?loc)))
             ((at ?pkg ?loc)))

  (:operator (!drive-truck ?truck ?loc-from ?loc-to ?city)
             ((at ?truck ?loc-from)
              (in-city ?loc-from ?city)
              (in-city ?loc-to ?city))
             ((at ?truck ?loc-from))
             ((at ?truck ?loc-to)
              (:protection (at ?truck ?loc-to))))

  (:operator (!fly-airplane ?airplane ?loc-from ?loc-to)
             ((at ?airplane ?loc-from))
             ((at ?airplane ?loc-from))
             ((at ?airplane ?loc-to)
              (:protection (at ?airplane ?loc-to))))


  ;; book-keeping methods & ops, to keep track of what needs to be done
  ;; !add-protection and !delete-protection are two special operators
  ;; that deal with the protection list instead of current state.
  (:operator (!!add-protection ?g)
             ()
             ()
             ((:protection ?g))
             0)

  (:operator (!!delete-protection ?g)
             ()
             ((:protection ?g))
             ()
             0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; main task
  (:method (at ?pkg ?loc-goal)
    package-already-there
    ((at ?pkg ?loc-goal))
    ()  ;; do nothing
    same-city-deliver
    ((at ?pkg ?loc-now)
     (in-city ?loc-now ?city-goal)
     (in-city ?loc-goal ?city-goal))
    (:ordered (:task in-city-delivery ?pkg ?city-goal ?loc-now ?loc-goal))
    different-city-deliver
    ((at ?pkg ?loc-now)
     (in-city ?loc-now ?city-now)
     (in-city ?loc-goal ?city-goal)   ;; ?city-goal != ?city-now
     (airport ?airport-now)
     (in-city ?airport-now ?city-now)
     (airport ?airport-goal)
     (in-city ?airport-goal ?city-goal))
    (:ordered (:task in-city-delivery ?pkg ?city-now ?loc-now ?airport-now)
              (:task air-deliver-obj ?pkg ?airport-now ?airport-goal)
              (:task in-city-delivery ?pkg ?city-goal ?airport-goal ?loc-goal)))


  ;; deliver a package within a city by a truck
  (:method (in-city-delivery ?pkg ?city ?loc-from ?loc-to)
    package-already-there
    ((same ?loc-from ?loc-to))
    ()  ;; do nothing
    truck-across-town-from-current-loc
    ((at ?truck ?loc-from)
     (truck ?truck))
    (:ordered (:task :immediate !!add-protection (at ?truck ?loc-from))
              (:task :immediate !load-truck ?pkg ?truck ?loc-from)
              (:task truck-at ?truck ?loc-to ?city)
              (:task :immediate !unload-truck ?pkg ?truck ?loc-to))
    truck-across-town-from-any-other-loc
    ((truck ?truck)
     (at ?truck ?loc-truck)
     (in-city ?loc-truck ?city))
    (:ordered (:task :immediate !drive-truck ?truck ?loc-from ?loc-to ?city)
              (:task :immediate !load-truck ?pkg ?truck ?loc-from)
              (:task truck-at ?truck ?loc-to ?city)
              (:task :immediate !unload-truck ?pkg ?truck ?loc-to)))


  ;; similar to !drive-truck.
  ;; but we also maintain the number of protected atoms
  (:method (truck-at ?truck ?loc-to ?city)
    truck-in-right-location
    ((at ?truck ?loc-to))
    (:ordered (:task :immediate !!add-protection (at ?truck ?loc-to)))
    truck-not-in-right-location
    ((at ?truck ?loc-from))  ;; ?loc-from != ?loc-to
    (:ordered (:task :immediate !drive-truck ?truck ?loc-from ?loc-to ?city)))


  ;; deliver a package between airports by a airplane
  (:method (air-deliver-obj ?pkg ?airport-from ?airport-to)
    fly-airplane-from-current-airport
    ((at ?airplane ?airport-from)
     (airplane ?airplane))
    (:ordered (:task :immediate !!add-protection (at ?airplane ?airport-from))
              (:task :immediate !load-airplane ?pkg ?airplane ?airport-from)
              (:task airplane-at ?airplane ?airport-to)
              (:task :immediate !unload-airplane ?pkg ?airplane ?airport-to))
    fly-airplane-from-any-other-airport
    ((at ?airplane ?any-airport)
     (airplane ?airplane))
    ; (:ordered (:task :immediate !fly-airplane ?airplane ?any-airport ?airport-from)
    (:ordered (:task :immediate !fly-airplane ?airplane ?any-airport ?airport-from)
              (:task :immediate !load-airplane ?pkg ?airplane ?airport-from)
              (:task airplane-at ?airplane ?airport-to)
              (:task :immediate !unload-airplane ?pkg ?airplane ?airport-to)))


  ;; similar to !fly-airplane.
  ;; but we also maintain the number of protected atoms
  (:method (airplane-at ?airplane ?airport-to)
    airplane-in-right-location
    ((at ?airplane ?airport-to))
    (:ordered (:task :immediate !!add-protection (at ?airplane ?airport-to)))
    airplane-not-in-right-location
    ((at ?airplane ?airport-from))  ;; ?loc-from != ?loc-to
    (:ordered (:task :immediate !fly-airplane ?airplane ?airport-from ?airport-to)))

  ; method for debugging
  ; (:method (println ?s1 ?s2)
  ;          ((eval (not (format t "~A~A~%" '?s1 '?s2)))) ())
  ; (:method (println ?s1 ?s2 ?s3)
  ;          ((eval (not (format t "~A~A~A~%" '?s1 '?s2 '?s3)))) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AXIOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:- (same ?x ?x) nil)
  (:- (different ?x ?y) ((not (same ?x ?y))))

;;;;;;;;;;;;;;;
;;; THE END ;;;
;;;;;;;;;;;;;;;

))

