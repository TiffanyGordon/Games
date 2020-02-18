;;;  CSC 458 - Final Project
;;;  Submitted by Ariana Gordon

;;;  The following program is designed to be a game of Go Fish between a human
;;;  player and the computer.  To play the game, load the file and type "(play)".
;;;  After the initial "(play)" command, parentheses are no longer needed when
;;;  interacting with the program.

;;;  The first several parameters and functions were selected from previous
;;;  assignments completed in this course. They will form the deck of cards to
;;;  be used, shuffle the deck, and deal the hands to both players.


(defparameter *suits* '(hearts diamonds clubs spades))
(defparameter *cards* '(ace king queen jack 10 9 8 7 6 5 4 3 2))

(defun make-deck (suits cards)
    (cond ((and (equal (list-length suits) 4) (equal (list-length cards) 13))
              (defparameter *deck* ())
              (defparameter *cardlist* (copy-list cards))
              (push (cons (car suits) (car cards)) *deck*)
              (make-deck suits (cdr cards)))
          ((and (equal (list-length suits) 1) (equal (list-length cards) 1))
              (push (cons (car suits) (car cards)) *deck*) *deck*)
          ((equal (list-length cards) 1)
              (push (cons (car suits) (car cards)) *deck*)
              (setq cards (copy-list *cardlist*))
              (make-deck (cdr suits) cards))
          (t  (push (cons (car suits) (car cards)) *deck*)
              (make-deck suits (cdr cards)))
    )
)

(defun shuffle (deck)
    (cond ((equal (list-length deck) 52)
                (defparameter *shuffled* ())
                (defparameter selected
                          (nth (random (list-length deck)) deck))
                   (push selected *shuffled*)
                   (shuffle (remove selected deck)))
          ((equal deck nil) *shuffled*)
          (t (defparameter selected
                    (nth (random (list-length deck)) deck))
             (push selected *shuffled*)
             (shuffle (remove selected deck)))
    )
)

(defun deal (deck hands cards)
  (shuffle deck)
  (defparameter *remaining* (copy-list *shuffled*))
  (defparameter *hand1* ())
  (defparameter *hand2* ())
  (loop for i from 1 to cards
      do (defparameter selected
                    (nth (random (list-length *remaining*)) *remaining*))
            (push selected *hand1*)
            (shuffle (delete selected *remaining*)))
   (loop for i from 1 to cards
      do (defparameter selected
                     (nth (random (list-length *remaining*)) *remaining*))
            (push selected *hand2*)
            (shuffle (delete selected *remaining*)))
   (list *hand1* *hand2*)
)

;;;  Below are some functions from Land of Lisp that were used in The Wizard's
;;;  Adventure Game to create a more user-friendly interaction with the computer
;;;  within the game.  These functions, particularly game-read and game-print,
;;;  have been utilized frequently within this program.

(defun game-repl ()
  (let ((cmd (game-read)))
      (unless (eq (car cmd) 'quit)
        (game-print (game-eval cmd))
        (game-repl))))

(defparameter *allowed-commands* '(2 3 4 5 6 7 8 9 10 jack queen king ace quit play "Go Fish" "Go fish" "go fish"))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(I do not know that command.)))

(defun game-read ()
  (let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")" ))))
        (flet ((quote-it (x)
                    (list 'quote x)))
              (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

              (defun game-eval (command)
                (if (member (car command) *allowed-commands*)
                    (eval command)
                    '(I do not know that command.)))

(defun tweak-text (lst caps lit)
    (when lst
    (let ((item (car lst))
        (rest (cdr lst)))
    (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
          ((eql item #\") (tweak-text rest caps (not lit)))
          (lit (cons item (tweak-text rest nil lit)))
          (caps (cons (char-upcase item) (tweak-text rest nil lit)))
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (output)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                            (prin1-to-string output))
                                'list)
                    t
                    nil)
            'string))
    (fresh-line))

;;;  The following function initiates game play.  It forms the deck of cards,
;;;  shuffles that deck, and deals a hand of 7 cards to each player.  It then
;;;  calls the function pinform.

(defun play ()
    (game-print '(You are starting a new game of "Go Fish."))
    (make-deck *suits* *cards*)
    (deal *deck* 2 7)
    (defparameter *drawpile* (shuffle *remaining*))
    (when (or (not (equal nil *hand1*)) (not (equal nil *hand2*)))
        (pinform)
    )
    (return)
)


;;;  This function, named pinform as an abbreviation for player information,
;;;  shows the player the cards in their hand and prompts them to request a
;;;  card value.  Whether the computer has a card of the requested value will
;;;  will be evaluated in a subsequent function.  When the player's turn has
;;;  concluded this function then calls on the computer to request a value to
;;;  add to it's hand and then initiates the player's response to the computer's
;;;  request.

(defun pinform ()
    (when (or (not (equal nil *hand1*)) (not (equal nil *hand2*)))
        (defparameter *ptable* '())
        (defparameter *vfreq1*(mapcar #'cons (pretty *hand1*) (itmfreq *hand1*)))
        (sort *vfreq1* #'> :key #'cdr)
        (game-print '(You are holding the following "cards:" ))
        (ph1)
        (spacing)
        (game-print '(What card value would you like to ask "for?"))
        (request)
        (spacing)
        (game-print '(My "turn!" Do you have a...))
        (compreq)
        (presponse)
    )
    (when (or (equal nil *hand1*) (equal nil *hand2*))
        (if (equal nil *hand1*)
            (game-print '(You don't have any more cards.))
            (game-print '(I am out of cards.)))
    )
)

;;;  This request function evaluates whether the computer has a card that
;;;  matches the player's request in hand or not.  If so, the computer will
;;;  give that card to the player and show the player it's updated hand.
;;;  Otherwise, the computer tells the player to go fish.

(defun request ()
    (defparameter *requested* (game-read))
    (defparameter *requested* (car *requested*))
    (defparameter handlength (length *hand1*))
    (loop for card in *hand2*
        do (cond ((equal *requested* (cdr card)) (push card *hand1*)
                                              (defparameter *hand2* (delete card *hand2*))
                                              (game-print '(You received a card!))
                                              (game-print '(You now have the following "cards:" ))
                                              (ph1))
        )
    )
    (when (equal (length *hand1*) handlength)
                    (game-print '(I do not have a matching card. Go fish!))
                    (fish)
    )
)

;;;  The fish function has the player draw cards from the undealt pile until a
;;;  card matching the player's requested value has been found.

(defun fish ()
    (if (not (equal *drawpile* '()))
        (loop for card in *drawpile*
            do (cond ((not (equal *requested* (cdr card))) (push card *hand1*)
                                           (defparameter *drawpile* (delete card *drawpile*)))
                   ((equal *requested* (cdr card)) (push card *hand1*)
                                                  (defparameter *drawpile* (delete card *drawpile*))
                                                  (game-print '(You found a "match!"))
                                                  (defparameter *vfreq1* (mapcar #'cons (pretty *hand1*) (itmfreq *hand1*)))
                                                  (sort *vfreq1* #'> :key #'cdr)
                                                  (ctable *hand1* *vfreq1*)
                                                  (game-print '(You now have the following "cards:" ))
                                                  (ph1)
                                                  (return))
            )
        )
        (game-print '(There are no cards left to draw from.)))
)

;;;  This function is the fish function for the computer.

(defun cfish ()
    (game-print '(I am fishing...))
    (loop for card in *drawpile*
        do (cond ((not (equal *askfor* (cdr card))) (push card *hand2*)
                                       (defparameter *drawpile* (delete card *drawpile*)))
               ((equal *askfor* (cdr card)) (push card *hand2*)
                                              (defparameter *drawpile* (delete card *drawpile*))
                                              (game-print '(I found a "match!"))
                                              (game-print `(I now have ,(length *hand2*) cards in my hand.))
                                              (game-print '(Your turn.))
                                              (return))
        )
    )
)

;;;  This function, named compreq to indicate computer request, organizes the
;;;  list of cards in the computer's hand to evaluate how many cards of each value
;;;  the computer is holding.  When requesting a card from the human player, this
;;;  function will ask for the value with the greatest number of cards in hand
;;;  after putting the 4-of-a-kind's on the table.

(defun compreq ()
    (defparameter *comptable* ())
    (defparameter *vfreq2* (mapcar #'cons (pretty *hand2*) (itmfreq *hand2*)))
    (sort *vfreq2* #'> :key #'cdr)
    (defparameter *askfor* (car (car *vfreq2*)))
    (game-print *askfor*)
)

;;;  This function specifies how the human player should respond to the computer's
;;;  card request.  If the player has 1 or more matching cards, they will be
;;;  displayed.  Otherwise, the player is prompted to tell the computer to go fish.

(defun presponse ()
    (defparameter *held* '())
    (loop for card in *hand1*
        when (equal (cdr card) *askfor*)
            do (push card *held*)
    )
    (cond ((equal (length *held*) 0) (game-print '(You do not have any matching "cards." Please tell me to Go "fish."))
                    (defparameter *cmd* (game-read))
                    (cfish))
            ((equal (length *held*) 1) (game-print '(You have 1 matching card.))
                    (game-print '(You are giving me ))
                    (print (car *held*))
                    (spacing)
                    (loop for card in *hand1*
                        do (when (equal card (car *held*))
                            (push card *hand2*)
                            (defparameter *hand1* (delete card *hand1*))))
                    (game-print '(Your turn.))
            )
            ((> (length *held*) 1) (game-print '(You have multiple cards that match. Which suit would you like to give?))
                    (princ *held*)
                    (fresh-line)
                    (defparameter *togive* (game-read))
                    (loop for card in *held*
                        do (if (equal (car card) *togive*)
                            (defparameter *togive* card)))
                    (loop for card in *hand1*
                        do (when (equal card *togive*)
                            (push card *hand2*)
                            (defparameter *hand1* (delete card *hand1*))))
                    (game-print '(Your turn.))
            )
    )
    (ctable *hand2* *vfreq2*)
    (pinform)
)

;;;  The following group of functions counts how many cards of each value are
;;;  in either player's hand and sorts them in descending order, then places any
;;;  4-of-a-kind's on the table.

(defun cdrlst (hand)
    (defparameter values (copy-list hand))
    (loop for item in values
        collect (cdr item)
    )
)

(defun pretty (hand)
    (remove-duplicates (cdrlst hand))
)

(defun itmfreq (hand)
    (loop for item in (pretty hand)
        collect (count item (cdrlst hand))
    )
)

(defun ctable (hand vfreq)
    (when (not (equal vfreq '()))
        (defparameter *cot*
            (loop for item in vfreq
                when (equal (cdr item) 4)
                collect (car item))
        )
        (when (not (equal *cot* '()))
            (if (equal hand *hand1*)
                (game-print '(You are holding 4 of a kind! The following values are now on the "table:" ))
                (game-print '(I am holding 4 of a kind! The following values are now on the "table:" )))
            (loop for item in *cot*
                do (game-print item))
            (loop for card in hand
                do (when (member (cdr card) *cot* :test #'equal)
                    (if (equal hand *hand1*)
                        (push card *ptable*)
                        (push card *comptable*))
                    (delete card hand)))
        )
    )
)

;;;  The following function prints the cards in the human player's hand.

(defun ph1 ()
    (loop for item in *hand1*
        do (print item))
)

;;;  The following function just serves as a line break to make the output prettier
;;;  and easier to read.

(defun spacing ()
    (game-print " ")
    (game-print " ")
)
