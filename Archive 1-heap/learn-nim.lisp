(load "given_func.lisp")

; AARON's
(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."
  ;;; IMPLEMENT ME
  ;; Q(s,a) to update
  (setf (aref q-table current-state action)
    (+
     ;; 1 - alpha * Q(s, a)
     (* (- 1 (funcall alpha-func iteration)) (aref q-table current-state action))
     ;; alpha * (r + gamma * highest q-value for best action in s')
     (* (funcall alpha-func iteration)
        (+ reward (* gamma (max-q q-table next-state))))))
  ;; return the q-table
  q-table)

; JOOST's
(defun q-learner2 (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."
;algorithm Q(s,a) <- (1 - alpha) * Q(s,a) + alpha * (reward + gamma * max_a'_Q(s', a'))
(setf tmp1 (* (- 1 (funcall alpha-func iteration)) (aref q-table current-state action)))
(setf tmp2 (* (funcall alpha-func iteration) (+ reward (* gamma (max-q q-table next-state)))))
(setf (aref q-table current-state action) (+ tmp1 tmp2))
q-table
)

(defun learn-nim (heap-size gamma alpha-func num-iterations)
    "Returns a q-table after learning how to play nim"
    (let ((q-table (make-q-table (+ heap-size 6) 3)))
        (dotimes (i num-iterations q-table)
            (do ((state 0))
                ((game-over-p state heap-size))
                (let ((old-state state) (reward 0) (action))
                    (setf action (max-action q-table state))
                    (incf state (1+ action))
                    (if (game-over-p state heap-size)
                        (setf reward -1)
                        (if (game-over-p
                            (incf state (random-elt '(1 2 3)))
                                heap-size)
                            (setf reward 1)
                        )
                    )
                    (setf q-table (q-learner q-table reward old-state action state gamma alpha-func i))
                )
            )
        )
    )
)
      
(defun game-over-p (state heap-size)
  "Returns true if the game state is end of the game"
  (>= state heap-size))


(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
 
  (dotimes (s (first (array-dimensions q-table)))
    (format t "~%s=~a:~a" s (max-action q-table s "-"))))
