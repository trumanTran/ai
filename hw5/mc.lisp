;;;; File:  mc.lisp
;;;; Author: Trung Tran

(defclass side-state (state) 
  ((boat-start
    :initarg :boat-start
    :initform nil
    :accessor boat-start
    :documentation "boat on start side of lake")
   (missionaries-start
    :initarg :missionaries-start
    :initform nil
    :accessor missionaries-start
    :documentation "missionaries on start side of lake")
   (cannibals-start
    :initarg :cannibals-start
    :initform nil
    :accessor cannibals-start
    :documentation "cannibals on start side of lake")
   (boat-goal
    :initarg :boat-goal
    :initform nil
    :accessor boat-goal
    :documentation "boat on goal side of lake")
   (missionaries-goal
    :initarg :missionaries-goal
    :initform nil
    :accessor missionaries-goal
    :documentation "missionaries on goal side of lake")
   (cannibals-goal
    :initarg :cannibals-goal
    :initform nil
    :accessor cannibals-goal
    :documentation "cannibals on goal side of lake")))

(defparameter *missionaries-and-cannibals*
  (make-instance 'problem
                 :start-state (make-instance 'side-state
                                             :boat-start t
                                             :missionaries-start 3
                                             :cannibals-start 3
                                             :boat-goal nil
                                             :missionaries-goal 0
                                             :cannibals-goal 0)
                 :goal-test 'all-at-goal-side
                 :operators '( two-cann-boat-to-goal 
                               two-miss-boat-to-goal
                               one-cann-boat-to-goal
                               one-miss-boat-to-goal
                               cann-and-miss-boat-to-goal
                               cann-and-miss-boat-to-start                               
                               one-cann-boat-to-start
                               one-miss-boat-to-start
                               two-cann-boat-to-start
                               two-miss-boat-to-start 
                             )
                 :name "missionaries-and-cannibals"))

(defmethod equal-states ((self side-state) (other side-state))
  (and (equal (boat-start self) (boat-start other))
       (equal (boat-goal self) (boat-goal other))
       (equal (missionaries-start self) (missionaries-start other))
       (equal (cannibals-start self) (cannibals-start other))
       (equal (missionaries-goal self) (missionaries-goal other))
       (equal (cannibals-goal self) (cannibals-goal other))))

(defmethod copy ((self side-state))
  (make-instance 'side-state
                 :boat-start (boat-start self)
                 :boat-goal (boat-goal self)
                 :missionaries-start (missionaries-start self)
                 :cannibals-start (cannibals-start self)
                 :missionaries-goal (missionaries-goal self)
                 :cannibals-goal (cannibals-goal self)))

; create goal state (3 missionaries and 3 cannibals at goal side along with boat)
(defmethod all-at-goal-side ((self side-state))
  (and (eql t (boat-goal self))
       (= (missionaries-goal self) 3)
       (= (cannibals-goal self) 3)))

(defmethod estimated-distance-from-goal ((self side-state))
  (declare (ignore self))
  1)

;;; define all operators 

(defmethod two-cann-boat-to-goal ((self side-state))
  (when  (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self))
          (old-miss-start (missionaries-start self)))
      (when 
          (and
           (>= old-cann-start 2)
           (or (=  old-miss-goal 0)
           (>= old-miss-goal (+ old-cann-goal 2))))                        
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (cannibals-start copy) (- old-cann-start 2))
        (setf (cannibals-goal copy) (+ old-cann-goal 2))
        copy))))

(defmethod two-miss-boat-to-goal ((self side-state))
  (when (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      
      (when (and
             (>=  old-miss-start 2)
             (<= old-cann-goal (+ old-miss-goal 2))
             (or (<= old-cann-start (- old-miss-start 2))
                 (= (- old-miss-start 2) 0))
             )
      (setf (boat-start copy) nil)
      (setf (boat-goal copy ) t)
      (setf (missionaries-start copy) (- old-miss-start 2))
      (setf (missionaries-goal copy) (+ old-miss-goal 2))
      copy))))

(defmethod one-cann-boat-to-goal ((self side-state))
  (when  (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self)))
      
      (when (and
             (>= old-cann-start 1)
             (or (< old-cann-goal old-miss-goal)
                 (= old-miss-goal 0))
             )
      (setf (boat-start copy) nil)
      (setf (boat-goal copy) t)
      (setf (cannibals-start copy) (- old-cann-start 1))
      (setf (cannibals-goal copy) (+ old-cann-goal 1))
      copy))))

(defmethod one-miss-boat-to-goal ((self side-state))
  (when  (eql (boat-start self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      
      (when (and
             (>= old-miss-start 1)
             (> old-miss-start old-cann-start)
             (<= old-cann-goal old-miss-goal)
             )
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (missionaries-start copy) (- old-miss-start 1))
        (setf (missionaries-goal copy) (+ old-miss-goal 1))
        copy))))

(defmethod cann-and-miss-boat-to-goal ((self side-state))
  (when  (eql (boat-start self) t)
             
    (let ((copy (copy self))
          (old-cann-start (cannibals-start self))
          (old-miss-start (missionaries-start self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self)))
     
      (when (and
             (>= old-miss-start 1)
             (>= old-cann-start 1)
             (>= old-miss-goal old-cann-goal)
             (>= old-miss-start old-cann-start)) 
        
        (setf (boat-start copy) nil)
        (setf (boat-goal copy) t)
        (setf (cannibals-start copy) (- old-cann-start 1))
        (setf (missionaries-start copy) (- old-miss-start 1))
        (setf (cannibals-goal copy) (+ old-cann-goal 1 ))
        (setf (missionaries-goal copy) (+ old-miss-goal 1))
        copy))))

(defmethod cann-and-miss-boat-to-start ((self side-state))
  (when (eql (boat-goal self) t)
     (let ((copy (copy self))
          (old-cann-goal (cannibals-goal self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-start (cannibals-start self))
          (old-miss-start (missionaries-start self)))
      (when
          (and
           (>= old-miss-goal 1)
           (>= old-cann-goal 1))
      (setf (boat-goal copy) nil)
      (setf (boat-start copy) t)
      (setf (cannibals-goal copy) (- old-cann-goal 1))
      (setf (missionaries-goal copy) (- old-miss-goal 1))
      (setf (cannibals-start copy) (+ old-cann-start 1))
      (setf (missionaries-start copy) (+ old-miss-start 1))
      copy))))

(defmethod one-cann-boat-to-start ((self side-state))
  (when  (eql (boat-goal self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self)))
      (when (and
             (>= old-cann-goal 1)
             (or (< old-cann-start old-miss-start)
                 (= old-miss-start 0)))
       
        (setf (boat-start copy) t)
        (setf (boat-goal copy) nil)
        (setf (cannibals-start copy) (+ old-cann-start 1))
        (setf (cannibals-goal copy) (- old-cann-goal 1))
        copy))))

(defmethod one-miss-boat-to-start ((self side-state))
  (when  (eql (boat-goal self) t)
    (let ((copy (copy self))
          (old-miss-goal (missionaries-goal self))
          (old-miss-start (missionaries-start self))
          (old-cann-goal (cannibals-goal self))
          (old-cann-start (cannibals-start self)))
      
      (when (and
             (>= old-miss-goal 1)             
             (or (> old-miss-goal old-cann-goal)
                 (= (- old-miss-goal 1) 0 ))
             (<= old-cann-start old-miss-start))
        (setf (boat-goal copy) nil)
        (setf (boat-start copy) t)
        (setf (missionaries-goal copy) (- old-miss-goal 1))
        (setf (missionaries-start copy) (+ old-miss-start 1))
        copy))))

(defmethod two-cann-boat-to-start ((self side-state))
  (when (eql (boat-goal self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self)))
      
      (when (and
             (> old-cann-goal 1)
             (or (= old-miss-start 0)
                 (> old-miss-start (+ old-cann-start 2))))
        (setf (boat-start copy) t)
        (setf (boat-goal copy) nil)
        (setf (cannibals-start copy) (+ old-cann-start 2))
        (setf (cannibals-goal copy) (- old-cann-goal 2))
        copy))))
        
(defmethod two-miss-boat-to-start ((self side-state))
  (when (eql (boat-goal self) t)
    (let ((copy (copy self))
          (old-miss-start (missionaries-start self))
          (old-cann-start (cannibals-start self))
          (old-miss-goal (missionaries-goal self))
          (old-cann-goal (cannibals-goal self)))

      (when  (>= old-miss-goal 2)
        (setf (boat-start copy) t)
        (setf (boat-goal copy ) nil)
        (setf (missionaries-start copy) (+ old-miss-start 2))
        (setf (missionaries-goal copy) (- old-miss-goal 2))
        copy))))


