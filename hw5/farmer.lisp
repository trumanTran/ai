;;;****************************************************************************************
;;;FUNCTIONS AND DEFINITIONS FOR FARMER FOX GOOSE AND GRAIN PROBLEM
;;;to be used concurrently with "search statistics" and a search file
;;;****************************************************************************************

(defclass farmer-state (state)
  ((farmer :initarg :farmer :initform nil :accessor farmer :documentation "the side the farmer is on")
   (fox :initarg :fox :initform nil :accessor fox :documentation "the side the fox is on")
   (goose :initarg :goose :initform nil :accessor goose :documentation "the side the goose is on")
   (grain :initarg :grain :initform nil :accessor grain :documentation "the side the grain is on")))

(defparameter *farmer* 
  (make-instance 'problem 
    :start-state (make-instance 'farmer-state :farmer 'L :fox 'L :goose 'L  :grain 'L)
    :goal-test 'all-on-rightp
    :operators '(farmer-takes-self farmer-takes-fox farmer-takes-goose farmer-takes-grain)
    :name "the farmer, the fox, the goose, and the grain"))

(defmethod all-on-rightp ((self farmer-state))
  (and (eql (farmer self) 'R)
       (eql (fox self) 'R)
       (eql (goose self) 'R)
       (eql (grain self) 'R)))

;;;****************************************************************************************
;;;SEARCH SUPPORT FUNCTIONS
;;;****************************************************************************************

(defmethod equal-states ((self farmer-state) (other farmer-state))
  (and (equal (farmer self) (farmer other))
       (equal (fox self) (fox other))
       (equal (goose self) (goose other))
       (equal (grain self) (grain other))))

(defmethod copy-state ((self farmer-state))
    (make-instance 'farmer-state :farmer (farmer self) :fox (fox self) :goose (goose self)
                   :grain (grain self)))

(defmethod estimated-distance-from-goal ((self farmer-state))
  (loop for item in '(farmer fox goose grain)
        count (equal (funcall item self) 'L)))

;;;****************************************************************************************
;;;OPERATORS AND THEIR SUPPORTING DEFINITIONS
;;;****************************************************************************************

(defun opposite (side)
  (cond ((eql side 'L) 'R)
        ((eql side 'R) 'L)))

(defun safe (state)
  (cond ((and (eql (fox state) (goose state))
              (not (eql (farmer state) (fox state))))
         nil)
        ((and (eql (grain state) (goose state))
              (not (eql (farmer state) (goose state))))
         nil)
        (t state)))

(defun farmer-takes-self (state)
  (let ((proposed (copy-state state)))
    (setf (farmer proposed) (opposite (farmer state))) 
    (safe proposed)))

(defun farmer-takes-fox (state)
  (when (eql (farmer state) (fox state))
    (let ((proposed (copy-state state)))
      (setf (farmer proposed) (opposite (farmer state)))
      (setf (fox proposed) (opposite (fox state)))
      (safe proposed))))

(defun farmer-takes-goose (state)
  (when (eql (farmer state) (goose state))
    (let ((proposed (copy-state state)))
      (setf (farmer proposed) (opposite (farmer state)))
      (setf (goose proposed) (opposite (goose state)))
      (safe proposed))))

(defun farmer-takes-grain (state)
  (when (eql (farmer state) (grain state))
    (let ((proposed (copy-state state)))
      (setf (farmer proposed) (opposite (farmer state)))
      (setf (grain proposed) (opposite (grain state)))
      (safe proposed))))