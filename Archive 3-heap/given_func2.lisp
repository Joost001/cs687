(defun random-elt (sequence)
    "Returns a random element from a sequence"
    (elt sequence (random (length sequence)))
)

(defun num-heaps (q-table)
    "Returns the number of heaps in a q-table"
    (- (length (array-dimensions q-table)) 1)
)

(defun num-states (q-table heap-id)
    "Returns the number of states in a q-table"
    (nth heap-id (array-dimensions q-table))
)

(defun num-actions (q-table)
    "Returns the number of actions in a q-table"
    (first (last (array-dimensions q-table)))
)

(defun make-3-heap-q-table (heap-sizes num-actions)
    "Makes a q-table, with initial values all set to 0"
    (make-array (list (first heap-sizes) (second heap-sizes) (third heap-sizes) num-actions) :initial-element 0)
)


(defun check-if-move-allowed (heap-sizes states action)
  (let* ((draw (+ (mod action 3) 1))
        (heap (values (floor (/ action 3))))
        (current-state (nth heap states))
       )
       (>= (nth heap heap-sizes) (+ current-state draw))
       ))


(defun max-q (q-table states heap-sizes)
    "Returns the highest q-value for a given state over all possible actions.
    If the state is outside the range, then utility-for-outside-state-range is returned."
    (let* ((num-actions (num-actions q-table)) 
           (best (aref q-table (first states) (second states) (third states) (random-elt '(0 1 2 3 4 5 6 7 8) )))
          )
        (dotimes (action num-actions best)
            (if (check-if-move-allowed heap-sizes states action)
                (setf candidate (aref q-table (first states) (second states) (third states) action))
                (setf candidate (- best 10))
              )
            (setf best (max candidate best))
          )
    )
)

(defun max-action (q-table states heap-sizes &optional val)
    "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
    else val is returned instead when there's a tie. If state is outside the range, then an error is generated
    (probably array-out-of-bounds)."
    (let ((num-actions (num-actions q-table)) (best (max-q q-table states heap-sizes)) bag)
    (dotimes (action num-actions)
        (when (= (aref q-table (first states) (second states) (third states) action) best)
          (push action bag)
        )
    )
    (if (and val (rest bag))
        val
        (if (rest bag)
            (random-elt bag)
            (random-elt '(0 1 2 3 4 5 6 7 8)) 
        )
    )
    )
)


(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

;TEST SECTION
; (setf heap-sizes (list 4 5 6))
; (setf q-table (make-3-heap-q-table heap-sizes 9))
; (setf states (list 0 0 0))
; (setf (aref q-table 0 0 0 5) 7.5)
; (print q-table)
; (print (num-heaps q-table))
; (print (num-states q-table 1))
; (print (num-actions q-table))

;TEST MAX-Q
; (setf num-actions (num-actions q-table))
; (print num-actions)
; (setf states (list 0 0 0))
; (print states)
; (print (aref q-table (first states) (second states) (third states) (1- num-actions)))
; (setf (aref q-table 0 0 0 5) 1)
; (setf best (aref q-table (first states) (second states) (third states) (1- num-actions)))
; (print best)
; (dotimes (action (1- num-actions) best)  ;; all but last action...
;       (setf best (max (aref q-table (first states) (second states) (third states) action) best)))
; (print best)
; (terpri)
; (terpri)
; (print (max-q q-table states))
; (print (max-action q-table states))