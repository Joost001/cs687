;;; This program implements an RL algo that learns to play nim. 
;;; The parameters to set are: heap-sizes, gamma, and the number of iterations

(defparameter *basic-alpha* 0.5 "A simple alpha constant")

(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

(defun random-elt (sequence)
    "Returns a random element from a sequence"
    (elt sequence (random (length sequence))))

(defun num-heaps (q-table)
    "Returns the number of heaps in a q-table"
    (- (length (array-dimensions q-table)) 1))

(defun num-states (q-table heap-id)
    "Returns the number of states in a q-table"
    (nth heap-id (array-dimensions q-table)))

(defun num-actions (q-table)
    "Returns the number of actions in a q-table"
    (first (last (array-dimensions q-table))))

(defun make-3-heap-q-table (state-sizes num-actions)
  "Makes a q-table, with initial values all set to 0"
    (make-array (list (first state-sizes) (second state-sizes) (third state-sizes) num-actions) :initial-element 0))

(defun check-if-move-allowed (heap-sizes states action)
  "Check if the action is allowed based on state and heap-size"
  (let* ((draw (+ (mod action 3) 1))
        (heap (values (floor (/ action 3))))
        (current-state (nth heap states)))
       (>= (nth heap heap-sizes) (+ current-state draw))))

(defun max-q (q-table states heap-sizes)
    "Returns the highest q-value for a given state over all possible actions.
    If the state is outside the range, then utility-for-outside-state-range is returned."
    (let* ((num-actions (num-actions q-table))
            (rand-action (random-elt '(0 1 2 3 4 5 6 7 8))) ;choose random action as a place filler
            (best (aref q-table (first states) (second states) (third states) rand-action )))
      
      ;if the random action is not allowed loop through and set the place filler to the min allowed action
      ;at this point we are still just trying to set the place filler.
      (if (not (check-if-move-allowed heap-sizes states rand-action))
	  (dotimes (action num-actions best)
	    (setf best (min (aref q-table (first states) (second states) (third states) action) best))))

      ;we loop through all the actions and if the action is allowed we compare it to best and take the max
        (dotimes (action num-actions best)
          (if (check-if-move-allowed heap-sizes states action)
	      (setf best (max best (aref q-table (first states) (second states) (third states) action)))))))

(defun max-action (q-table states heap-sizes &optional val)
    "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
    else val is returned instead when there's a tie. If state is outside the range, then an error is generated
    (probably array-out-of-bounds)."
    (let ((num-actions (num-actions q-table)) (best (max-q q-table states heap-sizes)) bag)
    (dotimes (action num-actions)
      (when (= (aref q-table (first states) (second states) (third states) action) best)
	(if (check-if-move-allowed heap-sizes states action)
          (push action bag))))
    (if (not bag)
	(push (random-elt '(0 1 2 3 4 5 6 7 8)) bag))
    (if (and val (rest bag))
	val
      (random-elt bag))))

(defun player-turn (q-table states heap-sizes)
  "Determine move, make move, and return state"
  (let* ((action (max-action q-table states heap-sizes))
	 (heap (values (floor (/ action 3)))))
    (setf (nth heap states) (+ (nth heap states) (mod action 3) 1))
    (list states action)))


(defun game-over-p (states heap-sizes)
  "Returns true if the game state is end of the game"
  (and
   (>= (first states) (first heap-sizes))
   (>= (second states) (second heap-sizes))
   (>= (third states) (third heap-sizes))))


(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration heap-sizes)
    "Modifies the q-table and returns it.  alpha-func is a function which must be called
    to provide the current alpha value."
    ;algorithm Q(s,a) <- (1 - alpha) * Q(s,a) + alpha * (reward + gamma * max_a'_Q(s', a'))
    (let ((alpha (funcall alpha-func iteration))
	  (tmp1)
	  (tmp2)
	  (cs0 (first current-state))
	  (cs1 (second current-state))
	  (cs2 (third current-state)))
      (setf tmp1 (* (- 1 alpha) (aref q-table cs0 cs1 cs2 action)))
      (setf tmp2 (* alpha (+ reward (* gamma (max-q q-table next-state heap-sizes)))))
      (setf (aref q-table cs0 cs1 cs2 action) (+ tmp1 tmp2))
      q-table))


(defun learn-nim (heap-sizes gamma alpha-func num-iterations)
"Returns a q-table after learning how to play nim"
(let* ((q-table (make-3-heap-q-table (mapcar #'(lambda(x) (+ 6 x) ) heap-sizes) 9))
       old-states p1-move p1-action p2-move reward)
  (dotimes (i num-iterations q-table)
    ;(format t "---interation:~d ~%" i)
    (do ((states (list 0 0 0)))
	((game-over-p states heap-sizes))

	;save state
	(setf old-states (copy-tree states))

	;player 1 determine best move, move, update state
	(setf p1-move (player-turn q-table states heap-sizes))
        (setf states (first p1-move))
        (setf p1-action (second p1-move))
        (if (game-over-p states heap-sizes)
            (setf reward -1))
        ;(format t "p1-move: ~d ~%" p1-move)
	
		;player 2 determine best move, move, update state
        (setf p2-move (player-turn q-table states heap-sizes))
        (setf states (first p2-move))
        (if (and (game-over-p states heap-sizes) (not (eql reward -1)))
            (setf reward 1))
        ;(format t "p2-move:~d ~%" p2-move)

		;set reward if game not ended
        (if (not (game-over-p states heap-sizes))
            (setf reward 0))

		;update q-table
        (setf q-table (q-learner q-table reward old-states p1-action states gamma alpha-func i heap-sizes))))
  q-table))

(setf q-table (learn-nim (list 5 6 7) .9 #'basic-alpha 10000))

(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))

(defun make-user-move (states)
    "Returns the number of sticks the user wants to remove"
    (let ((selected-heap)(selected-qty)(new-states (copy-tree states)))
        (loop
            (format t "~%Select heap to remove sticks(1,2,3)...  ")
            (setf selected-heap (1- (read)))
            (format t "~%Select number of sticks to remove(1,2,3)...  ")
            (setf selected-qty (read))
            (when (and (numberp selected-heap) (<= selected-heap 2) (>= selected-heap 0)  (numberp selected-qty) (<= selected-qty 3) (>= selected-qty 1))
                (setf (nth selected-heap new-states) (+  (nth selected-heap new-states) selected-qty)))
            (when (and (numberp selected-heap) (<= selected-heap 2) (>= selected-heap 0)  (numberp selected-qty) (<= selected-qty 3) (>= selected-qty 1))
                (return new-states))
            (format t "~%Answer must be between 1 and 3")
            )
        )
    )

(defun play-nim (q-table)
    "Plays a game of nim.  Asks if the user wants to play first,
    then has the user play back and forth with the game until one of
    them wins.  Reports the winner."
    (let ((heap-sizes (list 5 6 7)))

    (do ((states (list 0 0 0)) (is-user-turn (ask-if-user-goes-first))) ;; start game, initial is-user-turn boolean to prompt predicate
        ((game-over-p states heap-sizes) ;; play until heap is empty
        (if is-user-turn ;; if heap is empty, whoever's turn it is wins (the opponent took the last one)
            (print "User wins!")
            (print "AI wins!")))
        (format t "~%~a sticks remaining" (mapcar #'- heap-sizes states)) ;; print how many sticks remain to inform of current state

        (if is-user-turn 
            (setf states (make-user-move states))
            (setf states (first (player-turn q-table states heap-sizes)))
            )

        (setf is-user-turn (not is-user-turn)) ;; toggle whose turn it is
        )
        )
    )

(play-nim q-table)
