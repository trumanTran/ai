;;;****************************************************************************************
;;;THIS PROVIDES THE FOUNDATION FOR CLOS AI SEARCH
;;;to be used concurrently with a problem file
;;;****************************************************************************************

;;;****************************************************************************************
;;;SEARCH STATISTICS
;;;****************************************************************************************
(defclass search-statistics ()
  ((nodes-visited :initarg :nodes-visited :initform 0 :accessor nodes-visited)
   (maximum-length-of-node-list :initarg :maximum-length-of-node-list :initform 0 :accessor maximum-length-of-node-list)
   (length-of-solution :initarg :length-of-solution :initform 0 :accessor length-of-solution)
   (maximum-depth :initarg :maximum-depth :initform 0 :accessor maximum-depth)))
  
;;;****************************************************************************************
;;;CLASS DEFINITIONS
;;;****************************************************************************************

(defclass problem ()
  ((name :initarg :name :initform nil :accessor name)
   (start-state :initarg :start-state :initform nil :accessor start-state)
   (goal-test :initarg :goal-test :initform nil :accessor goal-test)
   (operators :initarg :operators :initform nil :accessor operators)
   (statistics :initarg :statistics :initform (make-instance 'search-statistics) :accessor statistics)))

(defmethod reset-statistics ((self problem))
  (setf (statistics self) (make-instance 'search-statistics)))
  
(defparameter *trace-search* nil)

;the following  should be redefined for each problem
(defclass state ()
  ())

(defmethod equal-states ((self state) (other state))
  ())

(defmethod copy-state ((self state))
  ())

(defmethod estimated-distance-from-goal ((self state))
  ())

(defmethod printer ((self state))
  ())

;;revised 1/14 to delete last 2 lines
(defclass node ()
  ((state :initarg :state :initform nil :accessor state)
   (problem :initarg :problem :initform nil :accessor problem)
   (path :initarg :path :initform nil :accessor path)
   (ancestors :initarg :ancestors :initform nil :accessor ancestors)))
   ;(estimated-distance-from-goal :initarg :estimated-distance-from-goal :initform nil :accessor estimated-distance-from-goal)
   ;(cost-of-plan-so-far :initarg :cost-of-plan-so-far :initform nil :accessor cost-of-plan-so-far)))

(defmethod update-statistics ((self problem) expand node-list)
  (let ((stats (statistics self)))
    (incf (nodes-visited stats))
    (when (> (length node-list) (maximum-length-of-node-list stats))
      (setf (maximum-length-of-node-list stats) (length node-list)))
    (when (> (length (path expand)) (maximum-depth stats))
      (setf (maximum-depth stats) (length (path expand))))))

;;;****************************************************************************************
;;;GENERAL FUNCTIONS 
;;;****************************************************************************************

;adds atom to the end of list
(defun add-to-end (atom list)
  (append list (list atom)))

;finds the successor of state resulting from application of operator
(defun successor-state (state operator)
  (funcall operator state))

;makes successor node from successor of state
(defmethod successor-node ((self node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 'node :state next :path (add-to-end operator (path self)) :problem (problem self)))))

(defmethod reached-the-goal ((self node))
  (funcall (goal-test (problem self)) (state self)))

;;;****************************************************************************************
;;;BREADTH FIRST SEARCH DEFINITIONS
;;;****************************************************************************************


(defmethod finish-successful-search ((self problem) expand)
  (setf (length-of-solution (statistics self)) (length (path expand)))
  (describe (statistics self))
  (format t "%")
  (describe expand)
  expand)

(defmethod breadth-first-search ((self problem)) 
  (let* ((initial-state (start-state self))
        (node-list (list (make-instance 'node :state initial-state :path nil :problem self)))
        (operators (operators self))
        (solved nil)
        (successors nil)
        (expand nil))
    (reset-statistics self) 
    (format t "~%Performing breadth first search on problem ~a.~%" (name self))
    (loop until (or (null node-list) solved)
          do (setf expand (pop node-list))
          (cond ((reached-the-goal expand) (setf solved t))
                (t (setf successors nil)
                   (loop for operator in operators
                         for next-node = (successor-node expand operator)
                         when next-node
                         do (setf successors (add-to-end next-node successors))
                         and do (update-statistics self next-node node-list)) 
                   (setf node-list (append node-list successors)))))
    (when solved (finish-successful-search self expand))))
  
(defparameter *trace-search* nil)

;;;****************************************************************************************
;;;DEPTH FIRST SEARCH DEFINITIONS
;;;****************************************************************************************

(defmethod depth-first-search ((self problem))
  (let* ((initial-state (start-state self))
         (node-list (list (make-instance 'node :state initial-state :path nil :problem self)))
         (operators (operators self))(solved nil)
         (successors nil)
         (expand nil))
    (reset-statistics self)
    (format t "~%Performing depth first search on problem ~a.~%" (name self))
    (loop until (or (null node-list) solved)
      do (setf expand (pop node-list))
      (update-statistics self expand node-list)
      (when *trace-search*
        (format t "~%~%Exploring ~a" (describe expand)))
      (cond ((reached-the-goal expand) (setf solved t))
            (t (setf successors nil)
               (loop for operator in operators
                 for next-node = (successor-node expand operator)
                 when next-node
                 do (setf successors (add-to-end next-node successors)))
               (setf node-list (append successors node-list)))))
    (when solved (finish-successful-search self expand))))

;there has to be an equal-states method defined
(defun already-visitedp (state visited)
  (member state visited :test 'equal-states))

;keep a list of states alredy visited
(defmethod depth-first-search-with-duplicate-node-detection ((self problem))
  (let* ((initial-state (start-state self))
         (node-list (list (make-instance 'node :state initial-state :path nil :problem self)))
         (operators (operators self)) (solved nil)
         (successors nil)
         (expand nil)
         (visited nil))
    (reset-statistics self)
    (format t "~%Performing depth first search with duplicate node detection on problem ~a.~%" 
            (name self))
    (loop until (or (null node-list) solved)
          do (setf expand (pop node-list))
          (setf visited (cons (state expand) visited))
          (update-statistics self expand node-list)
          (when *trace-search*
            (format t "~%~%Exploring ") (describe expand))
          (cond ((reached-the-goal expand) (setf solved t))
                (t (setf successors nil)
                   (loop for operator in operators
                         for next-node = (successor-node expand operator)
                         when (and next-node (not (already-visitedp (state next-node) visited)))
                         do (setf successors (add-to-end next-node successors)))
                   (setf node-list (append successors node-list)))))
    (when solved (finish-successful-search self expand))))

;set a global depth limit
(defparameter *default-depth-limit* 12)

;don't explore if length is greater than limit
(defmethod depth-first-search-with-depth-limit ((self problem))
  (let* ((initial-state (start-state self))
         (node-list (list (make-instance 'node :state initial-state :path nil :problem self)))
        (operators (operators self))
        (solved nil)
        (successors nil)
        (expand nil)
        (visited nil)
        (depth-limit *default-depth-limit*))
    (reset-statistics self)
    (format t "~%Performing depth first search with depth limit on problem ~a.~%" (name self))
    (loop until (or (null node-list) solved)
          do (setf expand (pop node-list))
          (setf visited (cons (state expand) visited))
          (update-statistics self expand node-list)
          (when *trace-search*
            (format t "~%~%Exploring ~a" (describe expand)))
          (cond ((reached-the-goal expand) (setf solved t))
                ((< (length (path expand)) depth-limit)
                 (setf successors nil)
                 (loop for operator in operators
                       for next-node = (successor-node expand operator)
                       when (and next-node (not (already-visitedp (state next-node) visited)))
                       do (setf successors (add-to-end next-node successors))
                 (setf node-list (append successors node-list))))))
    (when solved (finish-successful-search self expand))))

;;;****************************************************************************************
;;;ADDITIONAL BASIC DEFINITIONS FOR HEURISTIC SEARCH
;;;****************************************************************************************

(defclass heuristic-node (node)
  ((estimated-distance-from-goal :initarg :estimated-distance-from-goal
                                 :initform nil
                                 :accessor estimated-distance-from-goal)
   (cost-of-plan-so-far :initarg :cost-of-plan-so-far
                        :initform 0
                        :accessor cost-of-plan-so-far)))

(defun cost-of-applying-operator (state operator) 
  (declare (ignore state operator))
  1)

(defmethod successor-node ((self heuristic-node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 'heuristic-node :state next :path (add-to-end operator (path self))
        :estimated-distance-from-goal (estimated-distance-from-goal next)
        :problem (problem self)
        :cost-of-plan-so-far (+ (cost-of-plan-so-far self) 
                                (cost-of-applying-operator (state self) operator))))))

;;;****************************************************************************************
;;;BEST FIRST SEARCH DEFINITIONS
;;;****************************************************************************************

(defun sort-by-estimated-distance (list)
  (sort list #'< :key 'estimated-distance-from-goal))

(defmethod best-first-search ((self problem))
  (let* ((initial-state (start-state self))
         (open-list 
          (list (make-instance 'heuristic-node 
                  :state initial-state :path nil :problem self
                  :cost-of-plan-so-far 0
                  :estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
         (operators (operators self))
         (solved nil)
         (closed nil)
         (expand nil))
    (reset-statistics self)
    (format t "~%Performing best first search on problem ~a.~%" (name self))
    (loop until (or (null open-list) solved)
          do (setf expand (pop open-list)) 
          (setf closed (cons (state expand) closed)) 
          (cond ((reached-the-goal expand) (setf solved t))
                (t (loop for operator in operators
                         for next-node = (successor-node expand operator)
                         when (and next-node 
                                   (not (already-visitedp (state next-node) closed)))
                         do (update-statistics self next-node nil)
                         and do (push next-node open-list))
                   (setf open-list (sort-by-estimated-distance open-list)))))
    (when solved (finish-successful-search self expand))))
                        
;;;****************************************************************************************
;;;STEEPEST ASCENT HILL CLIMBING SEARCH DEFINITIONS
;;;****************************************************************************************

(defmethod steepest-ascent-hill-climbing ((self problem))
  (let* ((initial-state (start-state self))
         (current-node (make-instance 'heuristic-node :state initial-state 
                         :path nil :problem self
                         :cost-of-plan-so-far 0 
                         :estimated-distance-from-goal (estimated-distance-from-goal initial-state)))
         (operators (operators self))
         (solved nil)
         (best-successor nil)
         (minimum-distance-to-goal nil))
    (reset-statistics self)
    (format t "~%Performing steepest ascent hill climbing search on problem ~a.~%" (name self))
    (loop until (or (null current-node) solved)
      do (setf minimum-distance-to-goal (estimated-distance-from-goal current-node))
      (setf best-successor nil) 
      (cond ((reached-the-goal current-node) (setf solved t))
            (t (loop for operator in operators
                 for next-node = (successor-node current-node operator)
                 do (when next-node 
                      (update-statistics self next-node nil)) 
                 (when (and next-node
                            (< (estimated-distance-from-goal next-node) minimum-distance-to-goal))
                   (setf best-successor next-node) 
                   (setf minimum-distance-to-goal (estimated-distance-from-goal next-node))))
               (setf current-node best-successor)))) 
    (when solved (finish-successful-search self current-node))))

;;;****************************************************************************************
;;;OPTIMAL HEURISTIC SEARCH DEFINITIONS
;;;****************************************************************************************

(defun sort-by-cost-so-far (list)
  (sort list #'< :key 'cost-of-plan-so-far))

(defmethod optimal-heuristic-search ((self problem))
  (let* ((initial-state (start-state self))
         (open-list 
         (list (make-instance 'heuristic-node 
                 :state initial-state :path nil :problem self
                 :cost-of-plan-so-far 0
                 :estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
        (operators (operators self))
        (solved nil)
        (closed nil)
        (expand nil))
    (reset-statistics self)
    (format t "~%Performing optimal heuristic search on problem ~a.~%" (name self))
    (loop until (or (null open-list) solved)
          do (setf expand (pop open-list))
          (setf closed (cons (state expand) closed))
          (cond ((reached-the-goal expand) (setf solved t))
                (t (loop for operator in operators
                         for next-node = (successor-node expand operator)
                         when (and next-node 
                                   (not (already-visitedp (state next-node) closed)))
                         do (update-statistics self next-node nil)
                         and do (push next-node open-list))
                   (setf open-list (sort-by-cost-so-far open-list)))))
    (when solved (finish-successful-search self expand))))
                                                

;;;****************************************************************************************
;;;A-STAR SEARCH DEFINITIONS
;;;****************************************************************************************


(defclass a-star-node (heuristic-node)
  ())

(defmethod successor-node ((self a-star-node) operator)
  (let ((next (successor-state (state self) operator)))
    (when next
      (make-instance 
        'a-star-node :state next :path (add-to-end operator (path self)) :problem (problem self)
        :ancestors (cons self (ancestors self))
        :estimated-distance-from-goal (estimated-distance-from-goal next)
        :cost-of-plan-so-far (+ (cost-of-plan-so-far self) 
                                (cost-of-applying-operator (state self) operator))))))

(defun a-star-value (node)
  (+ (estimated-distance-from-goal node) (cost-of-plan-so-far node)))

(defun sort-by-cost-plus-plan (list)
  (sort list #'< :key 'a-star-value))

(defun equal-states-in-nodes (node-1 node-2)
  (equal-states (state node-1) (state node-2)))

;returns t if some old state on list has a longer path to node
(defun shorter-pathp (node list)
  (loop for old in list
        thereis (and (equal-states (state node) (state old))
                     (< (cost-of-plan-so-far node) (cost-of-plan-so-far old)))))

;returns 3 item list of 
;(nodes in list before one equivalent to node, equivalent node, list of nodes after)
(defun segment-path (node list)
  (let ((found nil)
        (equivalent nil)
        (answer nil))
    (setf answer
          (loop for old in list 
                when (and (not found) (equal-states (state node) (state old))
                          (< (cost-of-plan-so-far node) (cost-of-plan-so-far old)))
                do (setf found t)
                and do (setf equivalent old)
                else collect old into before
                when (and found (not (equal-states (state node) (state old))))
                collect old into after
                finally (return (list before equivalent after))))
    (if found answer nil)))

;corrects path and path length of any old on list that has equivalent to node in its ancestors
(defun correct-paths (node list)
  (loop for old in list
        for segments = (segment-path node (ancestors old))
        for equivalent = (second segments)
        for after = (third segments)
        unless (null equivalent)
        do (setf (ancestors old) (append (ancestors node) (list node) after))
        and do (setf (cost-of-plan-so-far old) 
                     (+ (cost-of-plan-so-far old) 
                        (cost-of-plan-so-far node) 
                        (- (cost-of-plan-so-far equivalent))))))

(defun already-visited-heuristicp (node visited)
  (member node visited :test 'equal-states-in-nodes))

;open-list and closed are both of nodes 
(defmethod a-star-search ((self problem))
  (let* ((initial-state (start-state self))
         (open-list 
          (list (make-instance 'a-star-node :problem self
                  :state initial-state :path nil
                  :cost-of-plan-so-far 0
                  :estimated-distance-from-goal (estimated-distance-from-goal initial-state))))
         (operators (operators self))
         (solved nil)
         (closed nil)
         (expand nil))
    (reset-statistics self)
    (format t "~%Performing a* search on problem ~a.~%" (name self))
    (loop until (or (null open-list) solved)
          do (setf expand (pop open-list))
         (when *trace-search*
           (format t "~%expanding ~a ~%" expand))
         (describe expand)
          (cond ((reached-the-goal expand) (setf solved t))
                (t (loop for operator in operators
                         for next-node = (successor-node expand operator)
                         when (and next-node 
                                   (not (already-visited-heuristicp next-node closed)))
                         do (update-statistics self next-node nil)
                         and do (push next-node open-list)
                         else when (and next-node (shorter-pathp next-node closed))
                         do (correct-paths next-node (append closed open-list)))
                   (setf open-list (sort-by-cost-plus-plan open-list))))
          (setf closed (cons expand closed)))
    (when solved (finish-successful-search self expand))))