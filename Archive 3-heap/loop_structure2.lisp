(load "./given_func2.lisp")
(load "./players2.lisp")

(defun game-over-p (states heap-sizes)
    "Returns true if the game state is end of the game"
    (AND 
        (>= (first states) (first heap-sizes))
        (>= (second states) (second heap-sizes))
        (>= (third states) (third heap-sizes))
    )
)

; Joost
(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration heap-sizes)
    "Modifies the q-table and returns it.  alpha-func is a function which must be called
    to provide the current alpha value."
    ;algorithm Q(s,a) <- (1 - alpha) * Q(s,a) + alpha * (reward + gamma * max_a'_Q(s', a'))
    (setf alpha (funcall alpha-func iteration))
    (setf tmp1 (* (- 1 alpha) (aref q-table (first current-state) (second current-state) (third current-state) action)))
    (setf tmp2 (* alpha (+ reward (* gamma (max-q q-table next-state heap-sizes)))))
    (setf (aref q-table (first current-state) (second current-state) (third current-state) action) (+ tmp1 tmp2))
    q-table

)

;TEST SECTION
(setf heap-sizes (list 4 4 4))
(setf states (list 0 0 0))
(setf num-iterations 10000)
(setf q-table (make-3-heap-q-table (mapcar #'(lambda(x) (+ 50 x) ) heap-sizes) 9))
(setf gamma 0.9)

(dotimes (i num-iterations q-table)
    ;(print 'new_iteration)
        (do ((states (list 0 0 0)))
            ((game-over-p states heap-sizes))

            ;(print 'next_inner_loop)
            ;(print state)

            ;save state
            (setf old-states (copy-tree states))

            ;player 1'
            ;(setf (values state p1-action) (values-list (player-turn q-table state 0)))
            (setf p1-move (player-turn q-table states heap-sizes))
            ;(print p1-move)
            (setf states (first p1-move))
            (setf p1-action (second p1-move))
            (if (game-over-p states heap-sizes)
                (setf reward -1)
                )

            ;player 2
            ;(setf (values state p2-action) (values-list (player-turn q-table state 1)))
            (setf p2-move (player-turn q-table states heap-sizes))
            ;(print p2-move)
            (setf states (first p2-move))
            (setf p2-action (second p2-move))
            (if (and (game-over-p states heap-sizes) (not (eql reward -1)))
                (setf reward 1)
                )

            (if (not (game-over-p states heap-sizes))
                (setf reward 0))

            ;update q-table
            (setf q-table (q-learner q-table reward old-states p1-action states gamma #'basic-alpha i heap-sizes))
            )
    )
