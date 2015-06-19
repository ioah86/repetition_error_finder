;;; repetition-error.el --- Interactive tools to find repetition errors in the buffer.

;; Copyright (C) 2015  Albert Heinle

;; Author: Albert Heinle <albert.heinle@googlemail.com>
;; Keywords: matching, convenience, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Description                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The main function in this package is "find-repetition-error". It
;; takes a starting point and an end point, and if the section is long
;; enough (i.e. enough words given), it will highlight repetitions,
;; and ask the user either to continue and ignore that repetition, or
;; to quit the search. In the latter case, the current cursor remains
;; at the position, and the user can correct the error, if he or she
;; wants.
;; There are the following functions that can be interactively
;; called.
;; find-repetition-error-whole-buffer:
;;   Runs the function "find-repetition-error" from the beginning
;;   until the end of the whole buffer
;; find-repetition-error-from-point:
;;   Runs the function "find-repetition error" from the current cursor
;;   position until the end of the document. This can be also used as
;;   a way to resume a previously stopped repetition error search
;;   (after the error has been corrected -- or not)
;;
;; Just try to call one of these functions, and the usage is quite
;; straight forward.
;;
;; What do our functions recognize as words:
;;   - Anything fulfilling the following regular expression:
;;     [a-zA-Z]{4,}, i.e. any trailing whitespaces or other symbols
;;     will be ignored.
;; How does the function move forward?
;;   - We always check a block of size 100 (default value, which can
;;     be changed by setting the variable
;;     repetition-error-word-block-size). If nothing is found in this
;;     block, the starting point moves forward by one word and tries
;;     again. This is repeated until either the last 100 words are reached
;;     and nothing was found, or until a repetition error has been
;;     revealed. If the user decides to ignore it and moves forward, the
;;     repeated word in that block will be saved with its position. In the
;;     moment, when the cursor moves forward, all ignored repeated words
;;     will be saved with the information about the block they have been
;;     encountered in. This is done because we don't want to repeatedly
;;     warn the user about repeated words in the same area, and these
;;     word will be ignored for all areas that intersect with the already
;;     considered and ignored one.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODOs:                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   - LaTeX support (i.e. ignore LaTeX keywords)
;;   - Experiment with different block sizes and repetition
;;     occurrences.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGELOG:                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;- Mi 14. Jan 22:30:15 EST 2015:
;;   Initial beta version done.
;;- Mo 2. Feb 11:04:53 EST 2015:
;;   - Added function to find repetition errors starting from current
;;     current cursor position.
;;   - removed save-recursion from find-repetition error function.
;;- Fr 6. Feb 23:23:57 EST 2015:
;;   - Altered the documentation a bit
;;   - General description given of the main functionality.
;;- Do 19. Feb 22:28:18 EST 2015:
;;   - added function get-next-n-words-with-ignore-list
;;   - added function is-point-in-ignore-list
;;   - changed regular expression in
;;     - get-next-n-words-with-ignore-list
;;     - get-next-n-words-from-point
;;- So 1. Mär 21:47:08 EST 2015:
;;   - changed the function get-next-n-words-with-ignore-list to
;;     actually not even put stuff from ignore-list into the resulting
;;     string. 
;;   - started to write the function
;;     create-ignore-list-for-latex-buffer
;;- So 12. Apr 01:27:47 EDT 2015:
;;   - continued working on create-ignore-list-for-latex-buffer;
;;     changes not tested yet though
;;- Fr 12. Jun 22:36:01 EDT 2015:
;;   - fixed an error in create-ignore-list-for-latex-buffer
;;   - made first test on "real" LaTeX-buffer
;;- Sa 13. Jun 21:06:57 EDT 2015:
;;   - Now two papers can be processed error-less by
;;     create-ignore-list-for-latex-buffer. Small bugfixes were
;;     needed. 
;;- Do 18. Jun 22:16:10 EDT 2015:
;;   - Altered the function find-repetition-error to now also include
;;     an optional parameter ignlist, the ignore-list for certain
;;     types of documents (e.g. LaTeX, etc.)
;;   - added (and rudimentary tested) the functions
;;        find-repetition-error-latex-from-point
;;        find-repetition-error-latex-whole-buffer


;;; CODE:


(defvar repetition-error-word-block-size 100
"
 (Positive Integer)
This variable determines the block size (i.e. the number of words)
our repetition-error-finding-routines will consider when trying to
find repetition errors. The initial value is 100
"
);repetition-error-word-block-size

(defvar repetition-error-min-occurrence 2
"
 (Positive Integer)
This variable determines the minimum number of repetitions for a certain
word to appear in a word-block to be considered a repetition error.
The initial value is 2.
"
);repetition-error-min-occurrence

(defun transform-complete-ci-string (s)
"string->string
This function consumes a string s, and replaces any letter in this
string to the regular expression accepting both the upper-case, as
well as the lower-case of that same letter. For example, the string
'ab' would be transformed into '[aA][bB]'.
"
  (replace-regexp-in-string "[[:alpha:]]"
	 (lambda (z) (concat "[" (downcase z) (upcase z) "]"))
	 s)
);transform-complete-ci-string

(defun find-repetition-error-whole-buffer ()
"None->None
This function will scan the whole buffer for repetitions of certain
words.  If the buffer does not have repetition-error-word-block-size
words (default: 100) then nothing is returned.  If in between, there
is a block found with repetition-error-word-block-size words and a
repetition of repetition-error-min-occurrence, then the repeated words
will be highlighted for the user. Then he or she can decide, if s/he
wants to go to the next repetition error or not using a
command-prompt.
SIDE-EFFECT:
 - Takes user inputs
 - Highlights text
"
  (interactive)
  (find-repetition-error (point-min) (point-max) repetition-error-word-block-size repetition-error-min-occurrence)
);find-repetition-error-whole-buffer

(defun find-repetition-error-latex-whole-buffer ()
"None->None
This function will scan the whole buffer for repetitions of certain
words, by ignoring LaTeX Commands.  If the buffer does not have repetition-error-word-block-size
words (default: 100) then nothing is returned.  If in between, there
is a block found with repetition-error-word-block-size words and a
repetition of repetition-error-min-occurrence, then the repeated words
will be highlighted for the user. Then he or she can decide, if s/he
wants to go to the next repetition error or not using a
command-prompt.
SIDE-EFFECT:
 - Takes user inputs
 - Highlights text
"
  (interactive)
  (find-repetition-error (point-min)
			 (point-max)
			 repetition-error-word-block-size
			 repetition-error-min-occurrence
			 (create-ignore-list-for-latex-buffer))
);find-repetition-error-latex-whole-buffer

(defun find-repetition-error-from-point ()
"None->None
This function will scan the whole buffer, starting from the current
cursor position, for repetitions of certain
words.  If the buffer does not have repetition-error-word-block-size
words (default: 100) then nothing is returned.  If in between, there
is a block found with repetition-error-word-block-size words and a
repetition of repetition-error-min-occurrence, then the repeated words
will be highlighted for the user. Then he or she can decide, if s/he
wants to go to the next repetition error or not using a
command-prompt.
SIDE-EFFECT:
 - Takes user inputs
 - Highlights text
"
  (interactive)
  (find-repetition-error (point) (point-max) repetition-error-word-block-size repetition-error-min-occurrence)
);find-repetition-error-whole-buffer

(defun find-repetition-error-latex-from-point ()
"None->None
This function will scan the whole buffer, starting from the current
cursor position, for repetitions of certain
words, by igoring LaTeX commands. If the buffer does not have repetition-error-word-block-size
words (default: 100) then nothing is returned.  If in between, there
is a block found with repetition-error-word-block-size words and a
repetition of repetition-error-min-occurrence, then the repeated words
will be highlighted for the user. Then he or she can decide, if s/he
wants to go to the next repetition error or not using a
command-prompt.
SIDE-EFFECT:
 - Takes user inputs
 - Highlights text
"
  (interactive)
  (find-repetition-error (point)
			 (point-max)
			 repetition-error-word-block-size
			 repetition-error-min-occurrence
			 (create-ignore-list-for-latex-buffer))
);find-repetition-error-latex-whole-buffer

(defun find-repetition-error (begin end &optional nWords minRep ignlist)
"integer->integer(->integer->integer->(listof (list integer integer)))->None
This function will scan the buffer between the character at position
begin and the character at position end for repetitions of certain
words. Begin and end are non-negative integers. Optionally, the user
can also define two integers nWords and minRep, which will make the
function return a finding, if there are more or equal minRep words
repeated in a block of nWords words. The default value for nWords is
100, the default value for minRep is 2, represented by the global
variables repetition-error-word-block-size and
repetition-error-min-occurrence.  If the block does not have nWords
words, then nothing is returned.  If in between, there is a block
found with nWords words and a repetition of minRep, then the repeated
words will be highlighted for the user. Then he or she can decide, if
s/he wants to go to the next repetition error or not using a
command-prompt.
Another optional parameter is ignlist. This is a list containing
intervals in which shall not be searched for repetition errors (There
are e.g. commands in LaTeX inside these intervals, etc...)
SIDE EFFECTS:
 - Takes user input
 - Highlights text
"
  (if (not nWords)
      (setq nWords repetition-error-word-block-size);then
  );if
  (if (not minRep)
      (setq minRep repetition-error-min-occurrence);then
  );if
  ;(save-excursion
  (if (<= end begin)
      "Invalid bounds"
    (goto-char begin)
    (recenter 0)
    (let 
	(;let definitions
	 (flag t)
	 (curWordBlock nil)
	 (exc nil)
	 (tempExc nil)
	 (tempKnownsList nil)
	 (usrcmd nil)
	);let definitions
      (while flag
	(if (not ignlist)
	    (setq curWordBlock (get-next-n-words-from-point nWords (point)))
	    (setq curWordBlock (get-next-n-words-with-ignore-list nWords (point) ignlist))
	);if
	(if
	  (equal curWordBlock "")
	  (progn
	    (setq flag nil)
	    "Reached the end of the buffer"
	  );progn for then
		;else
	  (setq exc (filter-known-words tempKnownsList (exceeders curWordBlock minRep) (point) (+ (point) (string-width curWordBlock))))
	  (setq tempKnownsList (update-knowns-list tempKnownsList exc (point) (+ (point) (string-width curWordBlock))))
	  (if (equal exc ())
	      (progn
		(re-search-forward "[[:space:]\n]" end t)
		(recenter 0)
		(if (> (point) end)
		  (setq flag nil)
		);if
	      );progn
		;else
	    (setq tempExc exc)
	    (while (not (equal tempExc ()))
	      (highlight-regexp (transform-complete-ci-string (car (car tempExc))))
	      (setq usrcmd 
		    (read-char (format "Repeated word: \"%s\". (c) Continue search for repetition errors or (any key) quit?" (car (car tempExc))))
	      );setq
	      (unhighlight-regexp (transform-complete-ci-string (car (car tempExc))))
	      (setq tempExc (cdr tempExc))
	      (if (not (equal usrcmd 99));;99 is ASCII for 'c'
                (progn
		  (setq flag nil)
		  (setq tempExc ())
		);progn
	      );if
	    );while
	    (re-search-forward "[[:space:]\n]" end t)
	    (recenter 0)
	    (if (> (point) end)
	      (setq flag nil)
	    );if
	  );if
	);if
      );while
    );let
  );if
  (message "Finished finding repetition errors")
  ;);save-excursion
);find-repetition-error

(defun update-knowns-list (knownList newExceeders leftBound
				     rightBound)
"(listof (list string int int))->(listof (list string int))->int->int->(listof (list string int))
This function consumes a list, knownlist, whose entries are tuples of a string and two integers,
a list with tuples of string and int, newExceeders, and two integers, leftBound and rightBound.
It first filters all the elements (s i j) in knownslist out, where j<leftbound. Then 
it returns a concatenation of the filtered knownList and a list containing for every (s i) in newExceeders a tuple
 (s leftBound rightbound).
ASSUMPTIONS:
 - newExceeders has no intersection with knownslist, given the left and the right bound.
"
  (let
      (;let definitions
       (tempNE newExceeders)
       (result (remove-if (lambda (m) (if (< (third m) leftBound) t nil)) knownList))
      );let definitions
    (while (not (equal tempNE ()))
      (setq result 
	    (cons (cons (first (first tempNE)) (cons leftBound (cons rightBound())))
		  result))
      (setq tempNE (rest tempNE))
    );while
    result
  );let
);update-knowns-list

(defun filter-known-words (knownList newExceeders leftBound rightBound)
"(listof (list string int int))->(listof (list string int))->int->int->(listof (list string int))
This function consumes a list, knownlist, whose entries are tuples of a string and two integers,
a list with tuples of string and int, newExceeders, and two integers, leftBound and rightBound.
It returns a filtered copy of the list newExceeders: If there is an entry (s i j) in knownList,
and the intervals [i,j] nad [leftBound, rightBound] have a nontrivial intersection, it will be omitted.
ASSUMPTIONS:
 - In every entry (s i j) of knownList, we always have i<j
 - leftBound < rightBound
"
  (labels
      (;labels definitions
       (compareToKnownList (kl el)
	  "This helper function consumes two lists, kl and el. kl
consists of 3 tuples, consisting of a string and two integers. el is a
list which first element is a string. This function returns true, if
the string in el does coincide with the string in at least one of the
elements in kl, and when this element's second and third element, say
 (u,v), has an intersection with (leftBound, rightBound) when viewing
them as intervals."
	  (let
	    (;let definitions
	       (tempKL kl)
	       (flag nil)
	    );let definitions
	    (while (and (not flag) (not (equal tempKL ())))
	      (if
	        (and
		   (equal (first (first tempKL)) (first el))
		   (< (second (first tempKL)) rightBound)
		   (> (third (first tempKL)) leftBound)
	        );and
		(setq flag t)
	      );if
	      (setq tempKL (rest tempKL))
	    );while
	    flag
	  );let
       );compareToKnownList
      );labels definitions
    (remove-if (lambda (el) (compareToKnownList knownList el)) newExceeders)
  );labels
);filter-known-words

;(filter-known-words '(("abc" 1 20) ("cde" 5 25)) '(("abc" 4) ("def" 2)) 19 30)
;;(filter-known-words '() '(("abc" 4) ("def" 2)) 19 30)
;; (first '(("abc" 1 20)))


(defun get-next-n-words-from-point (n p)
"Integer->Integer->string
Given an integer n and an integer p. The parameter p represents a
position in the buffer, n represents a number of words we want to
extract. This function returns a string containing the next n words
from point p in the buffer, if available. If there are no n words,
then the function returns the empty string.
ASSUMPTIONS:
 - The point n is at the beginning of a word
SIDE EFFECTS:
 - The cursor will in the end actually be moved to position p
 - Accesses cursor positions
"
  (goto-char p)
  (let
    (;let definitions
     (flag t)
     (i n)
     (curpos (point))
    );let definitions
    (while (and (> i 0) flag)
      (setq curpos (point))
      (re-search-forward "[[:space:]\n]+" (point-max) t)
      (if (equal curpos (point))
	  (setq flag nil)
      );if
      (setq i (- i 1))
    );while
    (goto-char p)
    (if flag
	(buffer-substring-no-properties p curpos)
        ""
    );if
  );let
);;get-next-n-words-from-point

(defun create-ignore-list-for-latex-buffer ()
"None->listof (Integer Integer)
This function scans the buffer for substrings which can be ignored by
our find-repetition-error routines, assuming that the current document
is a LaTeX file. In particular, this function will detect matches to
the following expressions and ignore them:
- Math-modes (\[.*\], \(.*\), $.*$, $$.*$$)
- \begin{.*} and \end{.*} 
- \.* in general (commands)
"
  (let
    (;let definitions
      (curpos 1)
      (result ())
      (newEntryL 0)
      (newEntryR 0)
      (foundFlag nil)
    );let definitions
    (while (< curpos (point-max))
      (setq foundFlag nil)
      (if (and 
	   (equal (string (char-after curpos)) "\\")
	   (equal (string-match "[a-zA-Z0-9]"
				(string (char-after (+ 1 curpos))))
		  0)
	  );and
        ;;In this case, we have encountered a command; it can be of
	;;the form \.* or \begin{.*}
        (progn
	  (setq foundFlag t)
	  (setq newEntryL curpos)
	  (setq curpos (+ curpos 1))
	  (while (equal (string-match "\\([a-zA-Z0-9{}]\\|\\[\\|\\]\\)"
			       (string (char-after curpos))) 0)
	    (setq curpos (+ curpos 1))
	  );while
	  (setq newEntryR curpos)
	  (setq result (cons (cons newEntryL (cons newEntryR ())) result))
        );progn
      );if
      (if (and 
	   (equal (string (char-after curpos)) "\\")
	   (or
	    (equal "[" (string (char-after (+ 1 curpos))))
	    (equal "(" (string (char-after (+ 1 curpos))))
	   );or
	   (not (equal (string (char-before curpos)) "\\"));; This
	   ;; catches the newlines that have a distance written with
	   ;; them, like e.g. \\[12pt]
	  );and
        ;;In this case, we have encountered math-mode via \[\] or \(\)
        (progn
	  (setq foundFlag t)
	  (setq newEntryL curpos)
	  (setq curpos (+ curpos 1))
	  (while (and
		  (not (equal (string (char-after curpos)) "\\"))
		  (or
		   (not (equal "]" (string (char-after (+ 1 curpos)))))
		   (not (equal ")" (string (char-after (+ 1 curpos)))))
		  );or
		 );and
	    (setq curpos (+ curpos 1))
	  );while
	  (setq curpos (+ 1 curpos))
	  (setq newEntryR curpos)
	  (setq result (cons (cons newEntryL (cons newEntryR ())) result))
        );progn
      );if
      (if (equal (string (char-after curpos)) "$")
	;;In this case, we have math mode initialized by $
	  (let
	    (;let definitions
	      (doubleDollar nil)
	    );let definitions
	    (setq foundFlag t)
	    (setq newEntryL curpos)
	    (if (equal (string (char-after (+ curpos 1))) "$")
	      (progn
		(setq doubleDollar t)
		(setq curpos (+ 1 curpos))
	      );progn
	    );if
	    (setq curpos (+ curpos 1))
	    (while (not (equal (string (char-after curpos)) "$"))
	      (setq curpos (+ curpos 1))
	    );while
	    (setq curpos (+ curpos 1))
	    (if doubleDollar
		(setq curpos (+ curpos 1))
	    );if
	    (setq newEntryR curpos)
	    (setq result (cons (cons newEntryL (cons newEntryR ())) result))
	  );let
      );if
      (if (not foundFlag)
	  (setq curpos (+ 1 curpos))
      );if
    );while
    (reverse result)
  );let
);create-ignore-list-for-latex-buffer ()

(defun is-point-in-ignore-list (p ign)
"Integer->listof (Integer Integer)->Boolean
Given an integer p, and a list of integer tuples ign.
If for a tuple (i j) in p we have that i<=p<=j, the function returns
t, and nil otherwise.
"
  (let
    (;let definitions
      (tempList (remove-if-not (lambda (m) (and (<= (first m) p) (<= p (second m)))) ign))
    );let definitions
    (if (equal tempList ())
      ;then
      nil
      ;else
      t
    );if
  );let
);is-point-in-ignore-list

;(is-point-in-ignore-list 3 '((5 6) (7 15) (20 75)))
;(is-point-in-ignore-list 10 '((5 6) (7 15) (20 75)))
;(is-point-in-ignore-list 3 ())

(defun get-next-n-words-with-ignore-list (n p ign)
"Integer->Integer->listof (list int int)->string
Given an integer n, an integer p, and a list of integer tuples ign.
The parameter p represents a
position in the buffer, n represents a number of words we want to
extract. This function returns a string containing the next n words
from point p in the buffer, if available. If for a tuple (i j) in
ignore, a word appears at position somewhere between i and j, it will
be ignored.
If there are no n words,
then the function returns the empty string.
ASSUMPTIONS:
 - The point n is at the beginning of a word
 - The beginning of a regexp is found after p, not before.
SIDE EFFECTS:
 - The cursor will in the end actually be moved to position p
 - Accesses cursor positions
"
  (goto-char p)
  (let
    (;let definitions
     (flag t)
     (i n)
     (curpos (point))
     (result "")
    );let definitions
    (while (and (> i 0) flag)
      (setq curpos (point))
      (re-search-forward "[[:space:]\n]+" (point-max) t)
      (if (equal curpos (point))
	  (setq flag nil)
      );if
      (if (not (is-point-in-ignore-list curpos ign))
	  (progn
	    (setq result (concat result
				 (buffer-substring-no-properties
				  curpos (point))))
	    (setq i (- i 1))
	  );progn
      );if
    );while
    (goto-char p)
    ;; (if flag
    ;; 	(buffer-substring-no-properties p curpos)
    ;;     ""
    ;; );if
    result
  );let
);;get-next-n-words-with-ignore-list

;(get-next-n-words-with-ignore-list 100 1 '((1 2000) (3000 4000)))

(defun exceeders (str number)
"string->integer->listof (list string integer)
Given a string str, and a number 'number', this function returns a
list of tuples (a, b), where a is a word in str, which appears b
times in str, where b>=number. Furthermore, for a, we ignore it
if its length is smaller than 4 letters, and we remove all space
characters, as well as digits and punctuation symbols.
"
  (labels 
      (;function definitions
       (count-each-word (l)
	 (let
	     (;let definitions
	      (tempList l)
	      (occ 0)
	      (result ())
	     );let definitions
	   (while (not (equal tempList ()))
	     (setq occ (count-if (lambda (m) (equal (downcase m) (downcase (car tempList)))) tempList))
	     (setq result (cons (cons (downcase (car tempList)) (cons occ ()))
				result))
	     (setq tempList (remove-if (lambda (m) (equal (downcase m) (downcase (car tempList)))) tempList))
	   );while
	   (nreverse result)
	 );let
	);count-each-word
       );;function definitions
    (;labels body
     let
      (;let definitions
       (filtered-inp (remove-if (lambda (m) (< (string-width m) 4))
				(split-string str
					      "[[:punct:][:digit:][:space:]\n]"
					      t)))
      );;let definitions
      (;let body
       remove-if (lambda (k) (< (second k) number)) (count-each-word
						     filtered-inp)
      );let body
    );labels body
  );;labels
);exceeders

(provide 'repetition-error)
;;; repetition-error.el ends here