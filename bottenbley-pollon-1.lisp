;;;; Reinforcement Learning Project
;;;; CS 687, GMU
;;;; Project 1
;;;; By Joost Bottenbley and Aaron Pollon

;This should not be particularly difficult, and you could probably get it done this week if you wished.
;A Q-learner that learns to play Nim.
;
;This project is an easy assignment.
;What I'm asking you to do is to complete the assignment, then
;"enhance" it in some way.  Try three heaps; try prioritorized sweeping
;(ask me); try comparing approaches to alpha, gamma, action-selection procedures,
;ways of implementing the "opponent, etc.  Try extending to 3-Heap Nim.
;Something fun.
;
;You might also try to analyze what happens when you try random opponents versus co-adaptive ones.  Advantages?  Disadvantages?
;
;ABOUT NIM
;---------
;There are several versions of Nim.  The version we will play is called 1-Heap Nim
;and it goes like this:
;
;1. Put N sticks in a pile (called a "heap")
;2. Players take turns taking 1, 2, or 3 sticks out of the heap.
;3. Whoever has to take out the last stick loses.
;
;
;LEARNING NIM
;------------
;
;Our Q-learner will build a Q-table solely through playing itself over and over
;again.  The table will tell it, ultimately, how smart it is to do a given move
;(take 1, 2, or 3 sticks) in a given state (number of sticks taken out so far).
;Q values will all start at 0.
;
;We will define the actions as follows:
;
;Action 0: take 1 stick out
;Action 1: take 2 sticks out
;Action 2: take 3 sticks out
;
;Thus the action number is exactly 1- the number of sticks to take out.  Keep
;this in mind -- the Q table will store Q values by action number, NOT by
;sticks taken out.
;
;We will define the states as follows:
;
;State 0: no sticks removed from heap
;State 1: 1 stick removed from heap
;...
;State N: N sticks removed from heap
;
;You will probably find it useful for the number of states in the Q table to
;be, believe it or not, about 6 larger than the heap size.  Thus there are
;some states at the high end of the table which represent, more or less,
;"negative heap sizes".  Of course, you can never play a negative heap size;
;such q-values will stay 0.
;
;Our Q table will be a STATE x ACTION array.  I have given you some functions
;which should make it easy to use this array:  NUM-STATES, NUM-ACTIONS,
;MAKE-Q-TABLE, MAX-Q, and MAX-ACTION.
;
;The Q learner will learn by playing itself: the learner records the current
;state, makes a move, lets the ``opponent'' make a move, then notes the new
;resulting state.  The action is the move the learner made.  Now we have s,
;a, and s'.  Note that s' is the state AFTER the opponent made his move.
;
;After the Q learner has learned the game, then you can play the learner
;and see how well it does.
;
;
;WHAT YOU NEED TO DO
;-------------------
;
;Your job is to implement several functions:
;
;Q-LEARNER
;  (the Q update function)
;LEARN-NIM
;  (the learning algorithm, tweaked for Nim -- the longest function)
;PLAY-NIM
;  (lets you play against the learned Q table)
;BEST-ACTIONS
;  (prints out the best actions believed so far)
;
;To help you, I've written a basic ALPHA function, and MAKE-USER-MOVE
;and ASK-IF-USER-GOES-FIRST functions.  I predict you will find them helpful.
;
;
;
;THE SECRET OF NIM (ugh, that was bad)
;-----------------
;
;You can get an idea for how well these settings perform by seeing what's
;usually the smallest number of iterations necessary before BEST-ACTIONS starts
;reporting the correct actions.
;
;So what ARE the correct actions in Nim?  There is a very simple rule for playing
;Nim.  If there are N sticks left in the pile, you want to remove sticks so that
;N = 1 + 4A where A is some number.  Then whatever your opponent takes out, you take
;4 minus that number, so your sticks and your opponent's sticks removed sum to 4.
;Keep on doing this, and eventually the A's will get dropped and your opponent will
;be left with 1 stick, which he must take.
;
;Depending on the size of the Nim heap, the game is either a guaranteed win for
;the first player or for the second player.  It all depends on who can get it down
;to 1 + 4A first.
;
;You will discover a certain pattern emerge in your BEST-ACTIONS list.  The first
;couple of values may be odd, but then from there on out you'll see
;2, 1, 0, <any>, 2, 1, 0, <any>, etc.  This is because in each of those heap
;values, the right move is to remove 3, 2, or 1 sticks, or (in the <any> value)
;it doesn't matter because you're guaranteed to lose at that heap size.  In essence
;you want to get your OPPONENT down to the <any> value (it's the 1 + 4A number).
;
;
;VERY STRONG HINT
;
;Keep in mind how the Q table is structured: actions are stored in the slot
;1 less than the number of sticks removed by that action.  And states go UP
;as more sticks are removed.   You may need to do some 1-'s and 1+'s to play
;the right action.
;
;
;INTERESTING TRIVIA
;
;Nim's been done a lot.  I was going to do tic-tac-toe, but decided it was too
;evil.  :-)

(load "./utilities.lisp")
(load "./queue.lisp")

(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
  "Returns the number of states in a q-table"
  (first (array-dimensions q-table)))

(defun num-actions (q-table &optional state)
  "Returns the number of actions in a q-table"
  (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
  "Makes a q-table, with initial values all set to 0"
  (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state)
  "Returns the highest q-value for a given state over all possible actions.
If the state is outside the range, then utility-for-outside-state-range is returned."
  (let* ((num-actions (num-actions q-table))
	 (best (aref q-table state (1- num-actions))))  ;; q of last action
    (dotimes (action (1- num-actions) best)  ;; all but last action...
      (setf best (max (aref q-table state action) best)))))

(defun max-action (q-table state &optional val)
  "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
else val is returned instead when there's a tie. If state is outside the range, then an error is generated
 (probably array-out-of-bounds)."
  ;; a little inefficient, but what the heck...
  (let ((num-actions (num-actions q-table))
	(best (max-q q-table state))
	bag)
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
	(push action bag)))
    (if (and val (rest bag))
	val
	(random-elt bag))))

(defun add-to-p-queue (p-queue state action priority)
  "Adds this state-action pair and its priorty to the priority queue, returns the updated queue."
  (enqueue-by-priority p-queue
		       ;; enqueue-by-prority requires a function
		       ;; so we'll store the state-action pair along with the priority
		       ;; lambda just retrieves the priority, makes it negative as lower number = higher priority
		       (lambda (x) (- (second x)))
		       (list (list state action) priority)))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

(defun dynamic-alpha (iteration)
  "Decreases alpha with each iteration, starting at 1"
  (max 0.0001 (- 1 (* iteration 0.0001))))

(defun player-turn (q-table state &optional (random-move 0))
  "Determine move"
  (if (= random-move 0)
      (max-action q-table state) ;determine move option 1
      (random-elt '(0 1 2)))) ;determine move option 0

(defun expert-player-turn (heap-size state)
  "Determine expert move"
  (let ((action (mod (- (- heap-size state) 1) 4)))
    ;; if Num. remaining - 1 mod 4 = 0, move doesn't matter
    (if (= action 0)
	(random-elt '(0 1 2))
	(1- action))))

(defparameter *opponent-expert-rate* 99) ; 100 = expert moves only, 0 = only random moves
(defun make-opponent-move (heap-size state)
  "Makes opponent move. Usually the expert move but every so often a random one to learn other paths
 (say against a 'dumb' opponent)"
  (if (> *opponent-expert-rate* (random 100))
      (expert-player-turn heap-size state)
      (random-elt '(0 1 2))))


(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."
  ;;; IMPLEMENT ME
  ;; Q(s,a) to update
  (let ((alpha (funcall alpha-func iteration)))
    (setf (aref q-table current-state action)
	  (+
	   ;; (1 - alpha) * Q(s, a)
	   (* (- 1 alpha) (aref q-table current-state action))
	   ;; alpha * (r + gamma * highest q-value for best action in s')
	   (* alpha (+ reward (* gamma (max-q q-table next-state)))))))
  ;; return the q-table
  q-table)

(defparameter *epsilon* 0.01) ; priority threshold for adding to p-queue
(defparameter *n* 1000) ; how many items to pull out of p-queue each move
(defun priortized-sweeping (q-table p-queue model gamma alpha)
  "Pulls state-action pairs out of priorit queue and processes. Returns the modified q-table."
  ;; Pull n items out, or until queue is empty
  (dotimes (i *n* q-table)
    (if (empty-queue? p-queue)
	(return q-table)
	(let* ((state-action (first (remove-front p-queue)))
	       (current-state (first state-action))
	       (action (second state-action))
	       (next-state-reward (gethash state-action model))
	       (next-state (first next-state-reward))
	       (reward (second next-state-reward)))
	  ;; Q(s,a) to update
	  (setf (aref q-table current-state action)
		(+
		 ;; 1 - alpha * Q(s, a)
		 (* (- 1 alpha) (aref q-table current-state action))
		 ;; alpha * (r + gamma * highest q-value for best action in s')
		 (* alpha (+ reward (* gamma (max-q q-table next-state))))))
	  ;; for nim we know all possible prev states, go through each possible action
	  (dotimes (prev-action 3)
	    (let* ((prev-state (- current-state (1+ prev-action)))
		   (state-reward-bar (gethash (list prev-state prev-action) model)))
	      ;; check if valid state and if we've been there before (and have a reward)
	      (if (and (>= prev-state 0) (not (null state-reward-bar)))
		  ;; calculate priority value
		  (let ((priority (abs (-
					(+ (second state-reward-bar) (* gamma (max-q q-table current-state)))
					(aref q-table prev-state prev-action)))))
		    ;; if priority exceeds threshold, add to queue
		    (if (> priority *epsilon*)
			(add-to-p-queue p-queue prev-state prev-action priority))))))))))

    
(defun q-learner-prioritized-sweeping (q-table p-queue model reward current-state action
				       next-state gamma alpha-func iteration)
  "Version of q-learner that implements prioritzed sweeping. Modifies the q-table and returns it."
  ;; update the state-reward model with this state action and the resulting state and reward
  (setf (gethash (list current-state action) model) (list next-state reward))
  ;; calculate the p-queue priority
  (let ((priority (abs (-
		(+ reward (* gamma (max-q q-table next-state)))
		(aref q-table current-state action)))))
    ;; does it exceed the threshold?
    (if (> priority *epsilon*)
	;; if so, add to queue
	(add-to-p-queue p-queue current-state action priority)))
  ;; call prioritized sweeping, which returns the updated q-table
  (priortized-sweeping q-table p-queue model gamma (funcall alpha-func iteration)))
  


;; Top-level nim learning algorithm.  The function works roughly like this...
;;
;; Make a q table.  Hint: make it 6 states larger than needed.  Can you see why?
;; Iterations times:
;;   Set state to 0  (no sticks removed from heap yet)
;;   Loop:
;;       old state <- state
;;       Determine and make my move, update state
;;       If I lost, set my reward to -1
;;       Determine and make my opponent's move, update state
;;       If the opponent lost, set my reward to +1
;;       If no reward has been set yet, set it to 0
;;       Update q table with the reward, old-state, my move, and current ("next") state
;;       If new state is bigger than the heap size, exit loop
;; Return q table

(defparameter *do-prioritized-sweeping* nil) ; enable prioritized sweeping
(defparameter *random-move-rate* 0.05) ; rate to select random move instead of best
(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"
  ;; create initial q-table, reward model, and priority queue
  (let ((q-table (make-q-table (+ heap-size 6) 3))
	(model (make-hash-table :test #'equalp))
	(p-queue (make-empty-queue)))
    ;; play nim "num-iterations" times
    (dotimes (i num-iterations q-table)
      ;; start a game	  
      (do ((state
	    ;; decide randomly if we should have opponent make a move first
	    (if (= 1 (random 2))
		(1+ (make-opponent-move heap-size 0)) ; opponent goes first
		0))) ; otherwise set state to 0 (we go first)
	  ;; play until heap is empty
	  ((game-over-p state heap-size))
	(let ((old-state state)
	      (reward 0)
	      ;; determine our best action from q-table and make it
	      ;; or sometimes make a random move instead for exploration
	      (action
		(if (< (/ (random 100) 100) *random-move-rate*)
		    (random-elt '(0 1 2))
		    (max-action q-table state))))
	  ;; action is 1- # sticks to remove
	  (incf state (1+ action))
	  ;; see if we lost
	  (if (game-over-p state heap-size)
	      (setf reward -1)
	      ;; make opponents move, then see if we won
	      (if (game-over-p (incf state (1+ (make-opponent-move heap-size state))) heap-size)
		  (setf reward 1)))
	  ;; update q-table
	  (setf q-table
		(if *do-prioritized-sweeping*
		    (q-learner-prioritized-sweeping q-table p-queue model reward old-state action state gamma alpha-func i)
		    (q-learner q-table reward old-state action state gamma alpha-func i))))))))
	  
(defun game-over-p (state heap-size)
  "Returns true if the game state is end of the game"
  (>= state heap-size))

(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))

(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
     (format t "~%Take how many sticks?  ")
     (setf result (read))
     (when (and (numberp result) (<= result 3) (>= result 1))
       (return result))
     (format t "~%Answer must be between 1 and 3"))))

(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,
then has the user play back and forth with the game until one of
them wins.  Reports the winner."

  ;;; IMPLEMENT ME
  ;; start game, initial is-user-turn boolean to prompt predicate
  (do ((state 0) (is-user-turn (ask-if-user-goes-first)))
      ;; play until heap is empty
      ((game-over-p state heap-size)
       ;; if heap is empty, whoever's turn it is wins (the opponent took the last one)
       (if is-user-turn
	   (print "User wins!")
	   (print "AI wins!")))
    ;; print how many sticks remain to inform of current state
    (format t "~%~a sticks remaining" (- heap-size state))
    ;; transition to next state, getting action based on whose turn
    (incf state (if is-user-turn
		 (make-user-move)
		 (1+ (max-action q-table state))))
    ;; toggle whose turn it is
    (setf is-user-turn (not is-user-turn))))


(defun best-actions (q-table)
  "Returns a list of the best actions. If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function
  (let ((actions ()))
    (dotimes (state (first (array-dimensions q-table)))
      ;; get the best action, mark a tie with -1
      (let ((action (max-action q-table state -1)))
	;; push to the acitons list
	(push
	 ;; if it's a tie or the best q value is negative, move doesn't matter
	 (if (or (= action -1) (> 0 (aref q-table state action)))
	     "-"
	     action)
	 actions)))
    (reverse actions))) ;; reverse since push adds to beginning
  

;; example:
;; 
;; (setq *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000))
;;
;; to get the policy from this table:
;;
;; (best-actions *my-q-table*)
;;
;; to play a game of your brain versus this q-table:
;;
;; (play-nim *my-q-table* 22)   ;; need to provide the original heap size
;;
;; You might try changing to some other function than #'basic-alpha...
