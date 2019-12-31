(defun null-board ()
  (list nil nil nil nil nil nil nil nil nil))
(defvar *start* nil)
(setq *start* (null-board))
(defun update-nth(state player n)
    (cond 
       ((null state) nil)
       ((= n 0) (cons player (cdr state)) )
       (t (cons (car state) (update-nth (cdr state) player (- n 1))))
       )
)
(defun get-b-states(state ls1)
  (dotimes (i 9)
     (if (equal (nth i state) nil) 
        (setf ls1 (append ls1 (list i)))
         
         ))
         (return-from get-b-states ls1))
         
(defun vs(ls)
    
 (if (and (equal (nth 0 ls) (nth 2 ls))
          (equal (nth 3 ls) (nth 5 ls))
          (equal (nth 6 ls) (nth 8 ls))
          )
       t
       nil
   )
   
)
(defun hs(ls)
    
 (if (and (equal (nth 0 ls) (nth 6 ls))
          (equal (nth 1 ls) (nth 7 ls))
          (equal (nth 2 ls) (nth 8 ls))
      )    
       t
       nil
   
  )
)
(defun d1s(ls)
    
 (if (and (equal (nth 1 ls) (nth 3 ls))
          (equal (nth 2 ls) (nth 6 ls))
          (equal (nth 5 ls) (nth 7 ls))
          )
       t
       nil
   )
)
(defun d2s(ls)
    
 (if (and (equal (nth 0 ls) (nth 8 ls))
          (equal (nth 1 ls) (nth 5 ls))
          (equal (nth 3 ls) (nth 7 ls))
          )
       t
       nil
   )
)

(defun del(x ls)
  (if (null ls) '()
   (if (= x (car ls)) 
      (cdr ls)
      (cons (car ls) (del x (cdr ls)))
      )))
(defun delete-vs(ls)
   (setf ls (del 2 ls))
   (setf ls (del 5 ls))
   (setf ls (del 8 ls))
   (return-from delete-vs ls)
)     
(defun delete-hs(ls)
   (setf ls (del 6 ls))
   (setf ls (del 7 ls))
   (setf ls (del 8 ls))
   (return-from delete-hs ls)
)
(defun delete-d1s(ls)
   (setf ls (del 3 ls))
   (setf ls (del 6 ls))
   (setf ls (del 7 ls))
   (return-from delete-d1s ls)
) 
(defun delete-d2s(ls)
   (setf ls (del 8 ls))
   (setf ls (del 5 ls))
   (setf ls (del 7 ls))
   (return-from delete-d2s ls)
)
(defun delete-sym-b(ls ls1)
   (cond ((hs ls) (setf ls1 (delete-hs ls1))))
   (cond ((vs ls) (setf ls1 (delete-vs ls1))))
   (cond ((d1s ls) (setf ls1 (delete-d1s ls1))))
   (cond ((d2s ls) (setf ls1 (delete-d2s ls1))))
   (return-from delete-sym-b ls1))
(defun get-nsym-b(ls)
   
   (delete-sym-b ls (get-b-states ls '()))
)
(defun next-move(state player)
   (get-children state (get-nsym-b state) player)
)
(defun get-children(state b-ls player)
  (cond ((null b-ls) nil)
        (t
          (cons (update-nth state player (car b-ls))
                (get-children state (cdr b-ls) player)
            )
         )
   )
)
(defun  is-goal(state player)
  (or (and (eq (nth 0 state) player) 
	       (eq (nth 1 state) player)
	       (eq (nth 2 state) player)
	   )
      (and (eq (nth 3 state) player) 
	       (eq (nth 4 state) player) 
	       (eq (nth 5 state) player)
	  )
      (and (eq (nth 6 state) player) 
           (eq (nth 7 state) player) 
	       (eq (nth 8 state) player)
	  )
      (and (eq (nth 0 state) player) 
	       (eq (nth 3 state) player) 
	       (eq (nth 6 state) player)
	  )
      (and (eq (nth 1 state) player)
	       (eq (nth 4 state) player) 
	       (eq (nth 7 state) player)
	  )
      (and (eq (nth 2 state) player) 
	       (eq (nth 5 state) player) 
	       (eq (nth 8 state) player)
	  )
      (and (eq (nth 0 state) player)
	       (eq (nth 4 state) player) 
	       (eq (nth 8 state) player)
	  )
      (and (eq (nth 2 state) player) 
	       (eq (nth 4 state) player) 
	       (eq (nth 6 state) player)
	   )
   )
)
(defun drawn?(state)
  (not (member nil state))
 )
(defun change-player(player)
 (if (eq player 'x)
     'o
     'x
  )
)

(defun print-board (b)
  (format t "~% ~d ~d ~d   0 1 2~% ~d ~d ~d   3 4 5~% ~d ~d ~d   6 7 8~%~%"
	  (or (nth 0 b) "-") (or (nth 1 b) "-") (or (nth 2 b) "-")
	  (or (nth 3 b) "-") (or (nth 4 b) "-") (or (nth 5 b) "-")
	  (or (nth 6 b) "-") (or (nth 7 b) "-") (or (nth 8 b) "-")))

(defparameter *ttt-lines* '((0 1 2)
                            (3 4 5)
                            (6 7 8)
                            (0 3 6)
                            (1 4 7)
                            (2 5 8)
                            (0 4 8)
                            (2 4 6)))


(defparameter *max-depth* 4)
(defun deep-enough (pos depth)
  (or (is-goal pos 'x) 
      (is-goal pos 'o)
      (drawn? pos)
      (>= depth *max-depth*)))

(defun static (pos player)
  (cond ((is-goal pos player) 10000)
        ((is-goal pos (change-player player)) -10000)
        ((drawn? pos) 0)
        (- (board-value pos player)
           (board-value pos player))))

(defun board-value(pos player)
  (loop for line in *ttt-lines*
        summing (line-score pos player line)))

(defun line-score(pos player line)
  (let* ((pieces (loop for point in line 
                      collect (nth point pos)))
        (x (count player pieces)))
    (if (member (change-player player) line)
      0
      x)))    
      
  (defvar *c* 0)  
  (defun minimax-a-b (pos depth player)
  (minimax-a-b-1 pos depth player 99999 -99999 t))

 (defun minimax-a-b-1 (pos depth player use-thresh pass-thresh return-move)
    (cond ((deep-enough pos depth)
             (setq *c* (+ *c* 1))
	       (unless return-move  (static pos player)))
	      (t 
	        (let ((successors (next-move pos player))
	              (best-move nil)
                  (quit nil)
                  (new-value nil))
          
               (declare (dynamic-extent successors))
	           (cond ((null successors) 
                      (setq *c* (+ *c* 1))
                      (unless return-move  (static pos player))
                      )
		             (t
		                (loop for succ in successors 
                                  until quit
                                do (setq new-value (- (minimax-a-b-1 
                                                                  succ 
                                                                  (+ 1 depth)
						                                          (change-player player)
						                                          (- pass-thresh)
						                                          (- use-thresh)
                                                                    nil)
                                                                    ))
                         (when (> new-value pass-thresh)
                               (setq pass-thresh new-value)
                               (setq best-move  succ)
                         )
                         (when (>= pass-thresh use-thresh) 
                          (setq quit t))
                        )
                        (if  return-move best-move pass-thresh)
                        )
                   )
               )
            )
       )
)
(defun play (&optional machine-first?)
  (let ((b *start*)
        (next nil))
    (setq *c* 0)
    (when machine-first? 
      (setq b (minimax-a-b b 0 'o)))
    (do ()
        ((or (is-goal b 'x) (is-goal b 'o) (drawn? b))
         (format t "Final position: ~%")
         (print-board b)
         (cond ((is-goal b 'o) (format t "I win.~%"))
               ((is-goal b 'x) (format t "You win.~%"))
               (t  (format t "Drawn.~%")))
        ; *c*
        )
      (print-board b)
      (format t #|~a |# "Your move: " #|*c*|#)
      (let ((m (read)))
        (loop until (setq next (update-nth b 'x m))
              do (format t "~%~a Illegal move, Try again: " m)
              (setq m (read)))
        (setq b next))              
      (when (and (not (drawn? b))
                 (not (is-goal b 'o))
                 (not (is-goal b 'x)))
        (print-board b)
        (setq b (minimax-a-b b 0 'o))
        (if (and (not (drawn? b))
                 (not (is-goal b 'o))) 
          (format t "My move: ~%"))))))



(defun self-play ()
  (let ((b *start*))
    (setq *c* 0)
    
    (do ()
        ((or (is-goal b 'x) (is-goal b 'o) (drawn? b))
         (format t "Final position: ~%")
         (print-board b)
         (cond ((is-goal b 'o) (format t "O win.~%"))
               ((is-goal b 'x) (format t "X win.~%"))
               (t  (format t "Drawn.~%")))
         ;*c*
         )
      (print-board b)
      (format t #|~a |# "X move: "  #|*c*|#)
      (setq b (minimax-a-b b 0 'x))              
      (when (and (not (drawn? b))
                 (not (is-goal b 'o))
                 (not (is-goal b 'x)))
        (print-board b)
        (setq b (minimax-a-b b 0 'o))
        (if (and (not (drawn? b))
                 (not (is-goal b 'o))) 
          (format t  #|~a |#" 0 move: " #|*c*|#))))))
(defun tic-tac-toe()
  (print "if you want to play first enter 1~%")
  (print "if you want the machine to play first enter 0~%")
  (setf b (read))
  (if (= b 1) 
      (play )
      (play t)
  )
)     
































