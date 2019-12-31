;;this is a function to print the board state in 3*3 matrix
;;0 1 2
;;3 4 5
;;6 7 8
(defun pretty-print (board)
  (format t "~%")
  (format t "~S ~S ~S~%" (nth 0 board) (nth 1 board) (nth 2 board))
  (format t "~S ~S ~S~%" (nth 3 board) (nth 4 board) (nth 5 board))
  (format t "~S ~S ~S~%" (nth 6 board) (nth 7 board) (nth 8 board)))

;;get-b-states is a function to get blank states of the board for a given board state
;;get-b-states returns list of blank state positions.
;;example: for empty board:- (0 1 2 3 4 5 6 7 8)
;;i/p:- ls is board state 
;;      ls1 is empty list
;;o/p :- ls1 containing list of blank states
(defun get-b-states(ls ls1)
  (dotimes (i 9)
     (if (equal (nth i ls) '-) 
        (setf ls1 (append ls1 (list i)))
         
         ))
         (return-from get-b-states ls1))
        
;;vs is a function to check whether the board state have vertical symmetry
;;i/p:- board state
;;o/p:- returns t or nil

(defun vs(ls)
    
 (if (and (equal (nth 0 ls) (nth 2 ls))
          (equal (nth 3 ls) (nth 5 ls))
          (equal (nth 6 ls) (nth 8 ls))
          )
       t
       nil
   )
   
)
;;hs is a function to check whether the board state have horizontal symmetry
;;i/p:- board state
;;o/p:- returns t or nil
(defun hs(ls)
    
 (if (and (equal (nth 0 ls) (nth 6 ls))
          (equal (nth 1 ls) (nth 7 ls))
          (equal (nth 2 ls) (nth 8 ls))
      )    
       t
       nil
   
  )
)
;;d1s is a function to check whether the board state have symmetry about diagonal 1
;;i/p:- board state
;;o/p:- returns t or nil
(defun d1s(ls)
    
 (if (and (equal (nth 1 ls) (nth 3 ls))
          (equal (nth 2 ls) (nth 6 ls))
          (equal (nth 5 ls) (nth 7 ls))
          )
       t
       nil
   )
)
;;d2s is a function to check whether the board state have symmetry about diagonal 2
;;i/p:- board state
;;o/p:- returns t or nil         
 
(defun d2s(ls)
    
 (if (and (equal (nth 0 ls) (nth 8 ls))
          (equal (nth 1 ls) (nth 5 ls))
          (equal (nth 3 ls) (nth 7 ls))
          )
       t
       nil
   )
)
;;del is a function to delete an element from a list
;;example: (del 2 (0 2 3 5 7)) returns (0 3 5 7)

(defun del(x ls)
  (if (null ls) '()
   (if (= x (car ls)) 
      (cdr ls)
      (cons (car ls) (del x (cdr ls)))
      )))
;;delete-vs deletes vertical symmetric positions 
;;eg: (0 3 6) (2 5 8) are symmetrical about vertical axis 
;; therefore deletes (2 5 8)  
(defun delete-vs(ls)
   (setf ls (del 2 ls))
   (setf ls (del 5 ls))
   (setf ls (del 8 ls))
   (return-from delete-vs ls)
)  
;;delete-hs deletes horizontal symmetric positions 
;;eg: (0 1 2) (6 7 8) are symmetrical about horizontal axis 
;; therefore deletes (6 7 8) 
(defun delete-hs(ls)
   (setf ls (del 6 ls))
   (setf ls (del 7 ls))
   (setf ls (del 8 ls))
   (return-from delete-hs ls)
)  
;;delete-d1s deletes diagonal 1 symmetric positions 
;;eg: (1 2 5) (3 6 7) are symmetrical about diagonal 1 axis 
;; therefore deletes (3 6 7) 
(defun delete-d1s(ls)
   (setf ls (del 3 ls))
   (setf ls (del 6 ls))
   (setf ls (del 7 ls))
   (return-from delete-d1s ls)
)  
;;delete-d2s deletes diagonal 2 symmetric positions 
;;eg: (0 1 3) (8 5 7) are symmetrical about diagonal 2 axis 
;; therefore deletes (8 5 7) 
(defun delete-d2s(ls)
   (setf ls (del 8 ls))
   (setf ls (del 5 ls))
   (setf ls (del 7 ls))
   (return-from delete-d2s ls)
)  
;;get-nsym-b is function which gives non symmetric blank positions for a given board state
;;eg:- for empty board it returns (0 2 4)
;;0 1 2
;;3 4 5
;;6 7 8       

(defun get-nsym-b(ls)
   
   (delete-sym-b ls (get-b-states ls '()))
)
;;delete-sym-b is a function which deletes one of the two symmetric positions ab0ut all axis of symmetry
;;i/p:-ls is board state
;;     ls1 is list of blank state postions
;;o/p:-ls1 after deleting all symmetrical blank state positions
(defun delete-sym-b(ls ls1)
   (cond ((hs ls) (setf ls1 (delete-hs ls1))))
   (cond ((vs ls) (setf ls1 (delete-vs ls1))))
   (cond ((d1s ls) (setf ls1 (delete-d1s ls1))))
   (cond ((d2s ls) (setf ls1 (delete-d2s ls1))))
   (return-from delete-sym-b ls1))
   
(defvar *max-player* 'X)
(defvar *min-player* 'O)


(defun make-struct (state parent depth hue player child)
   (list state parent depth hue player child))
   
(defun get-state (ls)
        (nth 0 ls))
(defun get-parent (ls)
        (nth 1 ls))
(defun get-depth (ls)
        (nth 2 ls))0
(defun get-hue (ls)
        (nth 3 ls))
(defun get-player (ls)
        (nth 4 ls)) 
 (defun get-child (ls)
      (nth 5 ls))       

;;defining global variables
(defvar *open* (list (make-struct (list '- '- '- '- '- '- '- '- '-) (list '- '- '- '- '- '- '- '- '-) 0 0 *max-player* nil)) )
(defvar *closed* '())
(defvar *open1* '())
(defvar  *closed1* '())
;;update-nth  updates nth element of the list
(defun update-nth(n elem ls)
    (cond 
       ((null ls) nil)
       ((= n 0) (cons elem (cdr ls)) )
       (t (cons (car ls) (update-nth (- n 1) elem (cdr ls)) ))
       )
)
;;is-goal-x checks board state returns t if x won
;;                                     nil otherwise
(defun is-goal-x (ls)
  (or 
       (and (equal (nth 0 ls) 'X) (equal (nth 1 ls) 'X) (equal (nth 2 ls) 'X))
       (and (equal (nth 3 ls) 'X) (equal (nth 4 ls) 'X) (equal (nth 5 ls) 'X))
       (and (equal (nth 6 ls) 'X) (equal (nth 7 ls) 'X) (equal (nth 8 ls) 'X))
       (and (equal (nth 0 ls) 'X) (equal (nth 3 ls) 'X) (equal (nth 6 ls) 'X))
       (and (equal (nth 1 ls) 'X) (equal (nth 4 ls) 'X) (equal (nth 7 ls) 'X))
       (and (equal (nth 2 ls) 'X) (equal (nth 5 ls) 'X) (equal (nth 8 ls) 'X))
       (and (equal (nth 0 ls) 'X) (equal (nth 4 ls) 'X) (equal (nth 8 ls) 'X))
       (and (equal (nth 2 ls) 'X) (equal (nth 4 ls) 'X) (equal (nth 6 ls) 'X))
   )) 
;;is-goal-x checks board state returns t if x won
;;                                     nil otherwise
(defun is-goal-o(ls)
   (or 
       (and (equal (nth 0 ls) 'O) (equal (nth 1 ls) 'O) (equal (nth 2 ls) 'O))
       (and (equal (nth 3 ls) 'O) (equal (nth 4 ls) 'O) (equal (nth 5 ls) 'O))
       (and (equal (nth 6 ls) 'O) (equal (nth 7 ls) 'O) (equal (nth 8 ls) 'O))
       (and (equal (nth 0 ls) 'O) (equal (nth 3 ls) 'O) (equal (nth 6 ls) 'O))
       (and (equal (nth 1 ls) 'O) (equal (nth 4 ls) 'O) (equal (nth 7 ls) 'O))
       (and (equal (nth 2 ls) 'O) (equal (nth 5 ls) 'O) (equal (nth 8 ls) 'O))
       (and (equal (nth 0 ls) 'O) (equal (nth 4 ls) 'O) (equal (nth 8 ls) 'O))
       (and (equal (nth 2 ls) 'O) (equal (nth 4 ls) 'O) (equal (nth 6 ls) 'O))
    ))
;;is-goal checks if board state is in goal state
;;         returns t or nil
(defun is-goal(ls)
     (or (is-goal-x ls)
          (is-goal-o ls))
          )
;;printing declaration at the end of the game
(defun print-declaration(ls)
    (cond ((is-goal-x ls)  (print "player 'X has won"))
          ((is-goal-y ls)  (print "player 'O has won")))
 )     
;;chane-player changes the player       
(defun change-player (player)
 (if (equal player *max-player*) *min-player*
      *max-player*
      ))
 

;;about-to-win checks if a given player is about to win
;;i/p :-player is player
;;      b-ls is non symmetrical blank state list 
;;      state is board state  
(defun about-to-win(player b-ls state)
  (cond  ((null b-ls) nil)
         ((is-goal (update-nth (car b-ls) player state))
           (cons (car b-ls) (about-to-win player (cdr b-ls) state))
           )
          (t
            (about-to-win player (cdr b-ls) state)
            )
           
     )

)


(defun get-children (ps b-ls depth player)
  (cond ((null b-ls) nil)
        (t 
          (cons (make-struct (update-nth (car b-ls) player ps) ps (+ 1 depth) 0 (change-player player) nil) (get-children ps (cdr b-ls) depth player))
          )
          ))
 
      
(defun bfs(n)
   (cond ((= (get-depth (car *open*)) n) (return-from bfs) )
        ((is-goal (get-state (car *open*))) 
                                               (if (equal (get-player (car *open*)) *min-player*) 
                                                        (setf (nth 3 (car *open*))  most-positive-fixnum)
                                                        (setf (nth 3 (car *open*))  most-negative-fixnum))
                                               (setf *closed* (cons (car *open*) *closed*))
                                               (setf *open*  (cdr *open*))
                                                     
                                                     (bfs n)
                                                     )
                                
                                                                                 
         ((null (new-states (get-children (caar *open*) (get-nsym-b (caar *open*)) (get-depth (car *open*))  (get-player (car *open*))) *open* *closed*))
                    (setf *closed* *closed*)
                    (setf *open* (cdr *open*))
                    (bfs n)
                    )
          (t
              ;(print "new-states")
              ;(print (new-states (get-children (caar *open*) (get-nsym-b (caar *open*)) (get-depth (car *open*))  (get-player (car *open*))) *open* *closed*))
               (setf *closed* (cons (car *open*) *closed*))
               (setf *open* (append (cdr *open*) (new-states (get-children (caar *open*) (get-nsym-b (caar *open*)) (get-depth (car *open*))  (get-player (car *open*))) *open* *closed*))
               )
         
           
             (bfs n)
             )
        ))

(defun hueristic(ls)
   (- (max-winning ls) (min-winning ls)
   )
)

(defun max-winning (ls)
     (setf a 0)
       (cond ((and (not(equal (nth 0 ls) 'O)) (not(equal (nth 1 ls) 'O)) (not(equal (nth 2 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 3 ls) 'O)) (not(equal (nth 4 ls) 'O)) (not(equal (nth 5 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 6 ls) 'O)) (not(equal (nth 7 ls) 'O)) (not(equal (nth 8 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 0 ls) 'O)) (not(equal (nth 3 ls) 'O)) (not(equal (nth 6 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 1 ls) 'O)) (not(equal (nth 4 ls) 'O)) (not(equal (nth 7 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 2 ls) 'O)) (not(equal (nth 5 ls) 'O)) (not(equal (nth 8 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 0 ls) 'O)) (not(equal (nth 4 ls) 'O)) (not(equal (nth 8 ls) 'O))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 2 ls) 'O)) (not(equal (nth 4 ls) 'O)) (not(equal (nth 6 ls) 'O))) (setf a (+ a 1))))
       
       (return-from max-winning a)
       )

(defun min-winning (ls)
     (setf a 0)
       (cond ((and (not(equal (nth 0 ls) 'X)) (not(equal (nth 1 ls) 'X)) (not(equal (nth 2 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 3 ls) 'X)) (not(equal (nth 4 ls) 'X)) (not(equal (nth 5 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 6 ls) 'X)) (not(equal (nth 7 ls) 'X)) (not(equal (nth 8 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 0 ls) 'X)) (not(equal (nth 3 ls) 'X)) (not(equal (nth 6 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 1 ls) 'X)) (not(equal (nth 4 ls) 'X)) (not(equal (nth 7 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 2 ls) 'X)) (not(equal (nth 5 ls) 'X)) (not(equal (nth 8 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 0 ls) 'X)) (not(equal (nth 4 ls) 'X)) (not(equal (nth 8 ls) 'X))) (setf a (+ a 1))))
       (cond ((and (not(equal (nth 2 ls) 'X)) (not(equal (nth 4 ls) 'X)) (not(equal (nth 6 ls) 'X))) (setf a (+ a 1))))
       
       (return-from min-winning a)
       )
(defun add-closed1(n)
  (cond ((null *closed*) nil)
        ((not (= (get-depth (car *closed*)) n)) (return-from add-closed1))
        (t
           (setf *closed1* (cons (car *closed*) *closed1* ))
           (setf *closed* (cdr *closed*))
           (add-closed1 n)
        )
   ))
(defun add-open1(p)
  (cond ((null *open*) nil)
        ((not (equal (get-parent (car *open*)) p)) (return-from add-open1))
        (t
          (setf *open1* (append *open1* (list (car *open*)) ) )
          (setf *open* (cdr *open*))
          (add-open1 p)
          )
  )) 
 
(defun update-hueristic()
   (dotimes (i (length *open*))
       (cond ( (is-goal (get-state (nth i *open*))) (if (equal (get-player (car *open*)) *min-player*)
                                                        (setf (nth 3 (nth i *open*))  most-positive-fixnum)
                                                        (setf (nth 3 (nth i *open*))  most-negative-fixnum)
                                                   ))
                                                  
             (t
               (setf (nth 3 (nth i *open*)) (hueristic (get-state (nth i *open*))))
               )
       )))
       
(defun is-member(state ls)
   (if (null ls) nil
       (if (equal state (nth 0 (car ls)) ) t
          (is-member state (cdr ls)) )
          ))
 
(defun new-states(c-ls ls1 ls2)
  (if (null c-ls) nil
   (if (or (is-member (nth 0 (car c-ls)) ls1)
           (is-member (nth 0 (car c-ls)) ls2)
           )
         (new-states (cdr c-ls) ls1 ls2)
         (cons (car c-ls) (new-states (cdr c-ls) ls1 ls2))
         )))
            
        
       
(defun appropriate(ls player)
   (if (equal player 'X)
        (minimum ls)
        (maximum ls)))           
        
(defun minimum(ls)
   (if (null (cdr ls)) (car ls)
      (min1 (car ls) (minimum (cdr ls)))))
(defun min1(a b)
   (if (<= (get-hue a) (get-hue b))
       a 
       b
       ))
(defun maximum(ls)
   (if(null (cdr ls)) 
            (car ls)
          (max1 (car ls) (maximum (cdr ls)) )
      ))
(defun max1(a b)
   (if (>= (get-hue a) (get-hue b))
       a 
       b
       ))
         
(defun backtrack()
    (cond ((null *closed1*) (return-from backtrack ) )
          ((is-goal (get-state (car *closed1*))) 
              
               (setf *open* (append *open* (list (car *closed1*)) ) )
               (setf *closed1* (cdr *closed1*))
               
              (backtrack )
               )
          (t 
              
            
             (add-open1 (get-state (car *closed1*)))
             (setf ls *open1*)
              ;(print "length *open1*")
              ;(print (length *open1*))
            (cond ((null ls) 
                    (setf *closed1* (cdr *closed1*))
                    (backtrack )
                    )
                   (t
                     (setf (nth 5 (car *closed1*)) (nth 0 (appropriate ls (get-player (car ls)) )) )
                     (setf (nth 3 (car *closed1*)) (nth 3 (appropriate ls (get-player (car ls)))))
                     (setf *open* (append *open* (list (car *closed1*))) )
                     (setf *closed1* (cdr *closed1*))  
                     (setf *open1* '())
                      (backtrack )
                    )
            )
           )
       )
)


(defun tic-tac-toe()
  (print "if you want to play first enter 1~%")
  (print "if you want the machine to play first enter 0~%")
  (setf b (read))
  (if (= b 1) 
      (minimax 1)
      (minimax 0)
  )
)

(defun minimax(player)
   (cond  ( (is-goal-x (nth 0 (car *open*)))
           (print "PLAYER X HAS WON")
           (return-from minimax )
          )
          ( (is-goal-o (nth 0 (car *open*)))
           (print "PLAYER O HAS WON")
           (return-from minimax )
          )
          ((null (get-nsym-b (nth 0 (car *open*)) ) )
           (print "BOARD IS FULL!!!!!!!!!~%")
           (print "MATCH DRAW------------------")
          )
          ( (= player 1)
           (game-player )
           (minimax 0)
          )
          ( (= player 0)
            (machine-player )
            (minimax 1)
          )
    )
)
    
(defun game-player()
 ; (print "-----------------------------------------------------------------~%")
  (pretty-print (nth 0 (car *open*)))
  (print "ENTER YOUR MOVE POSITION FROM 1 TO 9 WHICH IS NOT FILLED~%")         
  (setf c (read))
  (cond ((equal (nth (- c 1) (nth 0 (car *open*))) '-)
         (setf (nth (- c 1) (nth 0 (car *open*))) (nth 4 (car *open*))) 
         (setf (nth 4 (car *open*)) (change-player (nth 4 (car *open*)) ) )
         (setf (nth 2 (car *open*)) (+ 1 (nth 2 (car *open*))) )
         (setf (nth 1 (car *open*)) (nth 0 (car *open*)))
         ;(print *open*)
         (pretty-print (nth 0 (car *open*)) )
         (print "-------------------------")
         (return-from game-player )               
        )
        (t
          (print "ENTER VALID POSITION~%")
          (game-player )
         )
   )
  
)
(defun machine-player()
   (cond ((null (about-to-win (nth 4 (car *open*)) 
                             (get-nsym-b (nth 0 (car *open*)))
                             (nth 0 (car *open*))
                             )
                     )
     
            (cond ((null (about-to-win (change-player (nth 4 (car *open*)) )
                             (get-nsym-b (nth 0 (car *open*)))
                             (nth 0 (car *open*))
                             )
                     )
            ;(print *open*)  
            (setf d (nth 2 (car *open*)))
            (setf e 
                 (if (<= (+ d 4) 9)
                      (+ d 4)
                      9
                  )
            )
            ;(print "d")        
            ;(print d)
            ;(print "e")
            ;(print e)
            (bfs e)
            (update-hueristic )        
            (dotimes (j (- e d))
                (add-closed1 (- (- e 1) j))
                (backtrack )
            )
            ;(print *open*)
            (setf *open* (list (list (nth 5 (car *open*))
                                     (nth 5 (car *open*))
                                     (+ 1 (nth 2 (car *open*)))
                                     0
                                     (change-player (nth 4 (car *open*)))
                                     nil
                                 )
                          ))
            ;(print *open*)   
            ;(pretty-print (nth 0 (car *open*)) )
            (return-from machine-player )                    
           )   
           (t   
              (setf *open*  (list (list (update-nth  (car (about-to-win (change-player (nth 4 (car *open*)) )
                                                                       (get-nsym-b (nth 0 (car *open*)))
                                                                       (nth 0 (car *open*))
                                                                    ))
                                                    (nth 4 (car *open*))
                                                    (nth 0 (car *open*))
                                        )
                                       (update-nth  (car (about-to-win (change-player (nth 4 (car *open*)) )
                                                                       (get-nsym-b (nth 0 (car *open*)))
                                                                       (nth 0 (car *open*))
                                                                    ))
                                                    (nth 4 (car *open*))
                                                    (nth 0 (car *open*))
                                        )
                                      (+ 1 (nth 2 (car *open*)))
                                      0
                                      (change-player (nth 4 (car *open*))) 
                                      nil
                                      )
                                ))
              ;(pretty-print (nth 0 (car *open*)) )
              (return-from machine-player )
                              
            )
        ))
      (t
         (setf *open*  (list (list (update-nth  (car (about-to-win (nth 4 (car *open*)) 
                                                                       (get-nsym-b (nth 0 (car *open*)))
                                                                       (nth 0 (car *open*))
                                                                    ))
                                                    (nth 4 (car *open*))
                                                    (nth 0 (car *open*))
                                        )
                                       (update-nth  (car (about-to-win (nth 4 (car *open*))
                                                                       (get-nsym-b (nth 0 (car *open*)))
                                                                       (nth 0 (car *open*))
                                                                    ))
                                                    (nth 4 (car *open*))
                                                    (nth 0 (car *open*))
                                        )
                                      (+ 1 (nth 2 (car *open*)))
                                      0
                                      (change-player (nth 4 (car *open*))) 
                                      nil
                                      )
                                ))
              ;(pretty-print (nth 0 (car *open*)) )
              (return-from machine-player )
       
       )                            
              
    )
)
   
































            
 
