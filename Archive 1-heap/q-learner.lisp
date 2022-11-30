;; GIVEN FUNCTIONS FROM THE PROFESSOR
(load "./given_func.lisp")

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
    "Modifies the q-table and returns it.  alpha-func is a function which must be called
    to provide the current alpha value."
    ;algorithm Q(s,a) <- (1 - alpha) * Q(s,a) + alpha * (reward + gamma * max_a'_Q(s', a'))
    (setf alpha (funcall alpha-func iteration))
    (setf tmp1 (* (- 1 alpha) (aref q-table current-state action)))
    (setf tmp2 (* alpha (+ reward (* gamma (max-q q-table next-state)))))
    (setf (aref q-table current-state action) (+ tmp1 tmp2))
    q-table
)


; ; TEST AREA
; (setf heap-size 10)
; (setf reward -99)
; (setf q-table (make-q-table (+ heap-size 6) 3))
; (setf reward 1)
; (setf current-state 10)
; (setf action 0)
; (setf next-state 11)
; (setf gamma 0.1)
; (setf q-table (q-learner q-table reward current-state action next-state gamma #'basic-alpha 3))
; (print q-table)









