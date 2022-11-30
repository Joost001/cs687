(load "./given_func2.lisp")

(defun player-turn (q-table states heap-sizes)
    "Determine move, make move, and return state"
    (let ( (action (max-action q-table states heap-sizes))
           (heap (values (floor (/ action 3))) )
         )
        (setf (nth heap states) (+ (nth heap states) (mod action 3) 1))

        ; (setf action (max-action q-table states heap-sizes)) ; determine action
        ; (setf heap (values (floor (/ action 3))))
        ; (setf (nth heap states) (+ (nth heap states) (mod action 3) 1))
        (list states action))
    )

; TEST SECTION
(setf heap-sizes (list 4 5 6))
(setf q-table (make-3-heap-q-table heap-sizes 9))
(setf (aref q-table 0 0 0 5) 1)
(setf states (list 0 0 0))
(print q-table)
(setf p1-move (player-turn q-table states heap-sizes))
(print p1-move)