;;;; File:  hg.lisp
;;;; Author: Raymond Leung, Trung Tran, WeiJun Li

;global variables required for functions. Counter is used is keep track of the amount of colors CorrectColors has collected
;ElementsViewed and PermutationViewed are used as iterators to keep track of list position when player is giving output
;once all colors are found and placed in CorrectColors, all possible permutations are stored within AllPermutations  
(defvar *counter* 0)
(defvar *ElementsViewed* -1)
(defvar *CorrectColors* NIL)
(defvar *PermutationViewed* -1)
(defvar *AllPermutations* NIL)


;function used as an iterator for ElementsViewed
(defun ith-element (list)
  (nth *ElementsViewed* list))

;function used to find all permutations of CorrectColors. 
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

;analyze the score to determine whether points need to be added to counter and whether color guesses are correct and added into CorrectColors
;also used to reset all global variables should the last-response is nil (which means it's the beginning of a new game)
(defun analyze (last-response colors)

  (cond ((equal last-response nil)
	(setf *counter* 0)))
     
  (cond ((equal last-response nil)
	 (setf *ElementsViewed* -1)))

  (cond ((equal last-response nil)
	 (setf *CorrectColors* NIL)))

  (cond ((equal last-response nil)
	  (setf *PermutationViewed* -1)))

  (cond ((equal last-response nil)
	 (setf *AllPermutations* NIL)))
  
  (cond ((equal last-response nil))
	 ((= (first last-response) 0))
	 (T (setf *counter* (+ (first last-response) *counter*))))
  
  (cond ((equal last-response nil))
	((= (first last-response) 0))
	(T (loop for i from 1 to (first last-response) do (setf *CorrectColors* (cons (ith-element colors) *CorrectColors*))))))


;function used to find all correct colors by guessing monochromatically each turn until all are found
(defun guess-color (length colors)
  
  (loop for i from 1 to length collect (ith-element colors)))

;once all colors are found, a new list of AllPermutations is created so that all possible answers are stored to be guessed iteratively. 
;should there be duplicate answers within the list, it will be removed so that there's only distinct answers
(defun sort-color ()

  (cond ((equal *AllPermutations* nil)
	 (setf *AllPermutations* (all-permutations *CorrectColors*))
	 (setf *AllPermutations* (remove-duplicates *AllPermutations* :test #'equal))))

  (setf *PermutationViewed* (1+ *PermutationViewed*))

  (nth *PermutationViewed* *AllPermutations*))
  
 
;Holyguacamole player will guess all colors before trying each permutation of  the correct colors 
(defun HolyGuacamole (board colors SCSA last-response)
  
  (declare (ignore SCSA))
  
  (cond ((equal last-response nil)(analyze last-response colors))
        ((= (+ (first last-response) (nth 1 last-response)) board))
	(T (analyze last-response colors)))

  (cond ((= *counter* board)(sort-color))
	(t (setf *ElementsViewed* (1+ *ElementsViewed*))
	   (guess-color board colors))))
