;;; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;;;
;;; The code in this file was developed by Robert P. Goldman.
;;; Portions created by Dr. Goldman are Copyright (C) 2004 SIFT, LLC.
;;; These additions and modifications are made available by SIFT, LLC
;;; under the same licensing terms as the SHOP2 system as a whole.
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

(defun copy-state (state)
  (make-state-skel
   :tags-info (copy-tree (state-tags-info state))
   :statebody (copy-statebody state)
   :encoding (state-encoding state)))

(defun copy-statebody (state)
  (ecase (state-encoding state)
    ;; I am far from confident the cases for :mixed and :bit are
    ;; correct [2004/09/14:rpg]
    (:mixed (copy-hash-table (state-statebody state)))
    (:bit (BIT-make-statebody
     (list (copy-hash-table (first (state-statebody state)))
     (copy-hash-table (second (state-statebody state)))
     (copy-hash-table (third (state-statebody state)))
     (copy-tree (fourth (state-statebody state))))))
    (:list (copy-tree (state-statebody state)))
    (:hash (copy-hash-table (state-statebody state)))))

(defun copy-hash-table (H1 &optional (copy-fn #'identity))
  ;; modified this to use the hash-table-test function, instead of always building
  ;; an "EQUAL" hash-table.  Also initialized to be the same size, avoiding
  ;; resizes in building, I hope. [2002/10/08:rpg]
  (let ((H2 (make-hash-table :size (hash-table-size H1) :test (hash-table-test H1))))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
       H1)
    H2))

(defun state-trajectory (state)
  (let ((state (copy-state state)))
    (loop for state-info in (state-tags-info state)
  for state-list = (state-atoms state)
  with trajectory
  do (push state-list trajectory)
     (retract-state-changes state (first state-info))
  finally (return trajectory))))

