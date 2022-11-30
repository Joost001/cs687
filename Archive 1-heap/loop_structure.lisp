(load "./given_func.lisp")
(load "./players.lisp")

(defun game-over-p (state heap-size)
    "Returns true if the game state is end of the game"
    (>= state heap-size))

; Joost
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

; AARON
; (defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
;   "Modifies the q-table and returns it.  alpha-func is a function which must be called
; to provide the current alpha value."
;   ;;; IMPLEMENT ME
;   ;; Q(s,a) to update
;   (setf (aref q-table current-state action)
;     (+
;      ;; 1 - alpha * Q(s, a)
;      (* (- 1 (funcall alpha-func iteration)) (aref q-table current-state action))
;      ;; alpha * (r + gamma * highest q-value for best action in s')
;      (* (funcall alpha-func iteration)
;         (+ reward (* gamma (max-q q-table next-state))))))
;   ;; return the q-table
;   q-table)

(setf heap-size 12)
(setf num-iterations 10000)
(setf q-table (make-q-table (+ heap-size 6) 3))
(setf gamma 0.8)

(dotimes (i num-iterations q-table)
    ;(print 'new_iteration)
        (do ((state 0))
            ((game-over-p state heap-size))

            ;(print 'next_inner_loop)
            ;(print state)

            ;save state
            (setf old-state state)

            ;player 1'
            ;(setf (values state p1-action) (values-list (player-turn q-table state 0)))
            (setf p1-move(player-turn q-table state 0))
            ;(print p1-move)
            (setf state (first p1-move))
            (setf p1-action (second p1-move))
            (if (game-over-p state heap-size)
                (setf reward -1)
                )

            ;player 2
            ;(setf (values state p2-action) (values-list (player-turn q-table state 1)))
            (setf p2-move(player-turn q-table state 1))
            ;(print p2-move)
            (setf state (first p2-move))
            (setf p2-action (second p2-move))
            (if (and (game-over-p state heap-size) (not (eql reward -1)))
                (setf reward 1)
                )

            (if (not (game-over-p state heap-size))
                (setf reward 0))

            ;update q-table
            (setf q-table (q-learner q-table reward old-state p1-action state gamma #'basic-alpha i))


            )
    )

(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function

  ;;; IMPLEMENT ME
  (dotimes (s (first (array-dimensions q-table)))
    (format t "~%s=~a:~a" s (max-action q-table s "-"))))

(best-actions q-table)