(load "./given_func.lisp")

(defun player-turn (q-table state &optional (random-move 0))
    "Determine move, make move, and return state"
    (if (= random-move 0)
        (setf action (max-action q-table state)) ;determine move option 1
        (setf action (random-elt '(0 1 2))) ;determine move option 0
        )
    (setf state (+ state action 1)) ;make move
    (list state action)
    )

(defun expert-player-turn (heap-size state)
    "Determine move, make move, return"
    (setf remaining-sticks (- heap-size state))
    (setf action (mod (- remaining-sticks 1) 4))
    (if (= action 0)
        (setf action (random-elt '(1 2 3))))
    (setf state (+ state action))
    (setf action (- action 1))
    (list state action)
    )

; TEST SECTION
; (setf q-table (make-q-table 17 3))
; (setf state 0)
; (setf (aref q-table 0 1) 1)
; (setf (aref q-table 1 0) 1)
; (setf (aref q-table 3 2) 1)
; (setf (aref q-table 4 1) 1)
; (setf (aref q-table 5 0) 1)
; (player-turn q-table 0)
