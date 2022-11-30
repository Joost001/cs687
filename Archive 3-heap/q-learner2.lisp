;; GIVEN FUNCTIONS FROM THE PROFESSOR
(load "./given_func2.lisp")

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
    "Modifies the q-table and returns it.  alpha-func is a function which must be called
    to provide the current alpha value."
    ;algorithm Q(s,a) <- (1 - alpha) * Q(s,a) + alpha * (reward + gamma * max_a'_Q(s', a'))
    (setf alpha (funcall alpha-func iteration))
    (setf tmp1 (* (- 1 alpha) (aref q-table (first current-state) (second current-state) (third current-state) action)))
    (setf tmp2 (* alpha (+ reward (* gamma (max-q q-table next-state)))))
    (setf (aref q-table (first current-state) (second current-state) (third current-state) action) (+ tmp1 tmp2))
    (print tmp1)
    (print tmp2)
    q-table
)

;TEST AREA
; (setf heap-sizes (list 5 6 7))
; (setf reward -99)
; (setf q-table (make-3-heap-q-table heap-sizes 9))
; (setf reward 1)
; (setf current-state (list 0 0 0))
; (setf action 2)
; (setf next-state (list 3 0 0))
; (setf gamma 0.1)
; (setf q-table (q-learner2 q-table reward current-state action next-state gamma #'basic-alpha 3))
; (print q-table)
