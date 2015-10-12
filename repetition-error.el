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
;;- Sa 20. Jun 23:36:55 EDT 2015:
;;   - Reduced the complexity of the function
;;     find-repetition-error. If there is an ignore-list, we assume
;;     that it is sorted by the value of the first entry of each
;;     contained list; hence, we can ignore a whole bunch if our point
;;     has exceeded the entry in the ignore-list 
;;- Mi 1. Jul 00:37:29 EDT 2015:
;;   - Created function create-ignore-list-by-regexp
;;   - modified create-ignore-list-for-latex-buffer to also include
;;     math modes given by \begin{eqnarray[*]}..\end{eqnarray[*]}
;;     (resp. align[*])
;;- So 12. Jul 21:11:25 EDT 2015:
;;   - Changed the optimization in find-repetition-error with the
;;     ignore list back again, as I have ambiguous output. Maybe I will
;;     change that back later.
;;   - Added Comment support for create-ignore-list-for-latex-buffer
;;   - DEBUG for begin/end eqnarray.
;;   - found out about new interesting function "match-beginning" and
;;     replaced accordingly
;;   - Put some non-greedy-search for the align and
;;     eqnarray-Thing.
;;- Do 3. Sep 22:36:50 EDT 2015:
;;   - Edited the function create-ignore-list-for-latex-buffer, so
;;     that it uses create-ignore-list-by-regexp to find
;;     commented-out-areas.
;;   - get-next-n-words-with-ignore-list debugged and optimized
;;- Sa 10. Okt 14:55:38 EDT 2015:
;;   - Added tests for update-knowns-list (finally figured out how to
;;     test properly in emacs lisp)
;;   - Added tests for filter-known-words.
;;   - Added tests for is-point-in-ignore-list
;;   - Added Tests for exceeders
;;- So 11. Okt 22:07:23 EDT 2015:
;;   - Added tests for transform-complete-ci-string
;;   - Added test for get-next-n-words-from-point
;;- Mo 12. Okt 12:27:40 EDT 2015:
;;   - Moved the test-files into separate folder and altered the paths
;;     in the tests
;;   - Added tests for get-next-n-words-with-ignore-list
;;   - rewrite of the function get-next-n-words-with-ignore-list to
;;     catch some special cases


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
	 s
	 t)
);transform-complete-ci-string

(ert-deftest transform-complete-ci-string-test ()
"This function tests the fuction transform-complete-ci-string.
The covered test cases are:
1. Empty string
2. String with one letter, lowercase
3. String with one letter, uppercase
4. String with more than one letter, all lowercase
5. String with more than one letter, all uppercase
6. String with more than one letter, mixed upper and lower-case
"
  ;1.
  (should (equal (transform-complete-ci-string "") ""))
  ;2.
  (should (equal (transform-complete-ci-string "k") "[kK]"))
  ;3.
  (should (equal (transform-complete-ci-string "K") "[kK]"))
  ;4.
  (should (equal (transform-complete-ci-string "should")
		 "[sS][hH][oO][uU][lL][dD]"))
  ;5.
  (should (equal (transform-complete-ci-string "SHOULD")
		 "[sS][hH][oO][uU][lL][dD]"))
  ;6.
  (should (equal (transform-complete-ci-string "sHoulD")
		 "[sS][hH][oO][uU][lL][dD]"))
);transform-complete-ci-string-test


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
ASSUMPTIONS:
 - The elements in ignlist are sorted by their first entry.
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
	    ;else
	    ;; (while (and
	    ;; 	     (not (equal ignlist nil))
	    ;; 	     (< (first (first ignlist)) (point))
	    ;; 	   );and
	    ;;   ;in this loop, we will remove unnecessary entries in
	    ;;   ;ignlist, to reduce the complexity when executing
	    ;;   ;get-next-n-words-with-ignore-list
	    ;;   (setq ignlist (rest ignlist))
	    ;; );while
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
		(if ignlist
		  ;;in this case, we can move even further
		  (if (is-point-in-ignore-list (point) ignlist)
		    (progn
		      ;; (while (and
		      ;;     (not (equal ignlist nil))
		      ;;     (< (first (first ignlist)) (point))
		      ;;   );and
		      ;;   ;in this loop, we will remove unnecessary entries in
	              ;;   ;ignlist, to reduce the complexity when executing
	              ;;   ;get-next-n-words-with-ignore-list
	              ;;   (setq ignlist (rest ignlist))
	              ;; );while
		      (goto-char (second (is-point-in-ignore-list
					  (point) ignlist)))
		      (recenter 0)
		      (if (> (point) end)
		        (setq flag nil)
		      );if
		    );progn
		  );if
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
	    (if ignlist
	      ;;in this case, we can move even further
	      (if (is-point-in-ignore-list (point) ignlist)
	        (progn
	          ;; (while (and
	          ;;     (not (equal ignlist nil))
	          ;;     (< (first (first ignlist)) (point))
	          ;;   );and
	          ;;   ;in this loop, we will remove unnecessary entries in
	          ;;   ;ignlist, to reduce the complexity when executing
	          ;;   ;get-next-n-words-with-ignore-list
	          ;;   (setq ignlist (rest ignlist))
	          ;; );while
	          (goto-char (second (is-point-in-ignore-list (point) ignlist)))
	          (recenter 0)
	          (if (> (point) end)
	            (setq flag nil)
	          );if
	        );progn
	      );if
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
       (result (reverse (remove-if (lambda (m) (if (< (third m) leftBound) t nil)) knownList)))
      );let definitions
    (while (not (equal tempNE ()))
      (setq result 
	    (cons (cons (first (first tempNE)) (cons leftBound (cons rightBound())))
		  result))
      (setq tempNE (rest tempNE))
    );while
    (reverse result)
  );let
);update-knowns-list

;; Tests
(ert-deftest test-update-knowns-list ()
"Tests the function update-knowns-list. These are the covered test
cases:
1. both lists are empty
2. knownList is empty
3. newExceeders is empty and the result is knownslist
4. newExceeders is empty and there are entries in knownslist that are
   filtered.
5. Both lists are non-empty and in the end we get a clean
   concatenation of both.
6. Both lists are non-empty, but some entries in knownslist will be
   filtered.
"
  ;;1.
  (should (equal (update-knowns-list nil nil 0 1) nil))
  ;;2.
  (should (equal (update-knowns-list nil (list (list "hello" 0)
					       (list "kitty" 6)) 0
					       100)
		 (list (list "hello" 0 100)
		       (list "kitty" 0 100))))
  ;;3.
  (should (equal (update-knowns-list (list (list "hello" 0 50)
					   (list "kitty" 6 50)) nil 0
					   100)
		 (list (list "hello" 0 50)
		       (list "kitty" 6 50))))
  ;;4.
  (should (equal (update-knowns-list (list (list "hello" 0 50)
					   (list "kitty" 6 60)) nil 51
					   100)
		 (list (list "kitty" 6 60))))
  ;;5.
  (should (equal (update-knowns-list (list (list "hello" 0 50)
					   (list "kitty" 0 50))
				     (list (list "Dear Daniel" 70)
					   (list "Badtz-Maru" 85))
				     0 100)
		 (list (list "hello" 0 50)
		       (list "kitty" 0 50)
		       (list "Dear Daniel" 0 100)
		       (list "Badtz-Maru" 0 100))))
  ;;6.
  (should (equal (update-knowns-list (list (list "hello" 0 50)
					   (list "kitty" 0 60))
				     (list (list "Dear Daniel" 70)
					   (list "Badtz-Maru" 85))
				     51 100)
		 (list (list "kitty" 0 60)
		       (list "Dear Daniel" 51 100)
		       (list "Badtz-Maru" 51 100))))
);;test-update-knowns-list

(defun filter-known-words (knownList newExceeders leftBound rightBound)
"(listof (list string int int))->(listof (list string int))->int->int->(listof (list string int))
This function consumes a list, knownlist, whose entries are tuples of a string and two integers,
a list with tuples of string and int, newExceeders, and two integers, leftBound and rightBound.
It returns a filtered copy of the list newExceeders: If there is an entry (s i j) in knownList,
and the intervals [i,j] and [leftBound, rightBound] have a nontrivial
intersection, it will be omitted in newExceeders.
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

;; Tests

(ert-deftest test-filter-known-words ()
"Tests the function filter-known words. The covered test cases are the
following:
1. Both knownList and newExceeders are empty.
2. knownslist is empty, newExceeders is not.
3. knownsList is non-empty, newExceeders is empty
4. Both lists are non-empty, but nothing is filtered from
   newExceeders based on the fact that the words are different.
5. Both lists are non-empty, but nothing is filtered from newExceeders
   based on the fact that the interval in one entry in knownslist is
   not right.
6. Both lists are non-empty, and there is a filtering happening in
   newExceeders."
  ;;1.
  (should (equal (filter-known-words nil nil 0 100) nil))
  ;;2.
  (should (equal (filter-known-words nil '(("abc" 4) ("def" 2)) 19 30)
		 (list (list "abc" 4) (list "def" 2))))
  ;;3.
  (should (equal (filter-known-words '(("abc" 4 20) ("def" 2 35)) nil 19 30)
		 nil))
  ;;4.
  (should (equal (filter-known-words '(("abc" 4 20) ("def" 2 35))
				     '(("cde" 10) ("efg" 15)) 0 100)
		 '(("cde" 10) ("efg" 15))))
  ;;5.
  (should (equal (filter-known-words '(("abc" 1 18) ("cde" 5 25))
				     '(("abc" 4) ("def" 2)) 19 30)
		 '(("abc" 4) ("def" 2))))
  ;;6.
  (should (equal (filter-known-words '(("abc" 1 20) ("cde" 5 25))
				     '(("abc" 4) ("def" 2)) 19 30)
		 '(("def" 2))))
);test-filter-known-words

(defun get-next-n-words-from-point (n p)
"Integer->Integer->string
Given an integer n and an integer p. The parameter p represents a
position in the buffer, n represents a number of words we want to
extract. This function returns a string containing the next n words
from point p in the buffer, if available. If there are no n words,
then the function returns the empty string.
ASSUMPTIONS:
 - The point p is at the beginning of a word
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
    (setq curpos (point))
    (goto-char p)
    (if flag
	(buffer-substring-no-properties p curpos)
        ""
    );if
  );let
);;get-next-n-words-from-point

(ert-deftest get-next-n-words-from-point-test ()
"Here, we test the function get-next-n-words-from-point.
Our test suite contains the following test-cases:
1. An empty buffer
2. Boundary case for number of words with boundary that will produce
   text.
3. Boundary case for number of words with boundary that will not
   produce text.
4. Large text, boundary case producing text.
5. Large text, boundary case not producing text.
6. Large text, non-boundary case not producing text.
7. Large text, non-boundary case producing text.
"
  ;1.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-from-point 100 1) ""))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-from-point 3 1) "Lorem ipsum dolor.\n"))
  (kill-buffer "test_buffer_3_words.txt")
  ;3.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-from-point 4 1) ""))
  (kill-buffer "test_buffer_3_words.txt")
  ;4.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 50 1) "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
sed diam voluptua. At vero eos et accusam et justo duo dolores et ea
rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem
ipsum dolor sit amet.
"))
  (kill-buffer "test_buffer_50_words.txt")
  ;5.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 51 1) ""))
  (kill-buffer "test_buffer_50_words.txt")
  ;6.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 100 1) ""))
  (kill-buffer "test_buffer_50_words.txt")
  ;7.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 3 1) "Lorem ipsum dolor "))
  (kill-buffer "test_buffer_50_words.txt")
);get-next-n-words-from-point-test

(defun create-ignore-list-by-regexp (inpRE)
"string->listof (Integer Integer)
This function consumes a regular expression inpRE, and finds it in the
current buffer. For every found regexp, it produces its beginning
point and end-point, and puts these coordinates in a list. A list of
all these tuples is returned in the end.
"
  (save-excursion
    (goto-char (point-min))
    (let
       (;let definitions begin
         (curpos (point))
         (flag t)
	 (result ())
	 (tempLeft 0)
	 (tempRight 0)
       );let definitions end
       (while flag
	 (re-search-forward inpRE (point-max) t)
	 (if (equal curpos (point))
	     (setq flag nil)
	   ;else
	   (setq tempRight (point))
	   (setq tempLeft (match-beginning 0))
	   (setq curpos (point))
	   (setq result (cons (cons tempLeft (cons tempRight ())) result))
	 );if
       );while
       (reverse result)
    );let
  );save-excursion
);create-ignore-list-by-regexp

;; Tests
(ert-deftest create-ignore-list-by-regexp-test ()
"This is a collection of tests for create-ignore-list-by-regexp.
The covered test cases are:
1. empty file, empty regex
2. empty file, nonempty regex
3. non-empty file, empty regex
4. regex is not in non-empty file
5. regex is in non-empty file exactly once.
6. regex is in non-empty file more than once."
  ;1.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (create-ignore-list-by-regexp "") nil))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (create-ignore-list-by-regexp "[a-zA-Z]+") nil))
  (kill-buffer "empty_test_buffer.txt")
  ;3.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (create-ignore-list-by-regexp "") nil))
  (kill-buffer "test_buffer_50_words.txt")
  ;4.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (create-ignore-list-by-regexp "[0-9]+!$") nil))
  (kill-buffer "test_buffer_50_words.txt")
  ;5.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (create-ignore-list-by-regexp "Lorem") '((1 6)) ))
  (kill-buffer "test_buffer_3_words.txt")
  ;6.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (create-ignore-list-by-regexp "Lorem") '((1 6) (269 274)) ))
  (kill-buffer "test_buffer_50_words.txt")
);create-ignore-list-by-regexp-test

(defun create-ignore-list-for-latex-buffer ()
"None->listof (Integer Integer)
This function scans the buffer for substrings which can be ignored by
our find-repetition-error routines, assuming that the current document
is a LaTeX file. In particular, this function will detect matches to
the following expressions and ignore them:
- Math-modes (\[.*\], \(.*\), $.*$, $$.*$$)
- \begin{.*} and \end{.*} 
- \.* in general (commands)
- math modes a la \begin{eqnarray[*]} .. \end{eqnarray[*]} or
  \begin{align*} .. \end{align{*}}
- Comments beginning with '%' and going until the end of the line
GENERAL ASSUMPTIONS:
- The returned ignore-list is sorted by the first element of each
  contained list.
"
  (let
    (;let definitions
      (curpos 1)
      (result ())
      (newEntryL 0)
      (newEntryR 0)
      (foundFlag nil)
    );let definitions
    (setq result (append result (create-ignore-list-by-regexp
				 "[\\]begin{eqnarray[\*]?}\\(.\\|\n\\)+?[\\]end{eqnarray[\*]?}")))
    (setq result (append result (create-ignore-list-by-regexp
				 "[\\]begin{align[\*]?}\\(.\\|\n\\)+?[\\]end{align[\*]?}")))
    (setq result (append result (create-ignore-list-by-regexp
				 "%.*?$")))
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
	  (while (and
		   (equal (string-match "\\([a-zA-Z0-9{}]\\|\\[\\|\\]\\)"
			       (string (char-after curpos))) 0)
		   (< curpos (- (point-max) 1))
		 )
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
    (sort (reverse result) (lambda (x y) (<= (first x) (first y))))
  );let
);create-ignore-list-for-latex-buffer ()

(defun is-point-in-ignore-list (p ign)
"Integer->listof (Integer Integer)->(Integer Integer)
Given an integer p, and a list of integer tuples ign.
If for a tuple (i j) in p we have that i<=p<=j, the function returns
the first interval in ign with p in it, and nil otherwise.
"
  (let
    (;let definitions
      (tempList (remove-if-not (lambda (m) (and (<= (first m) p) (<= p (second m)))) ign))
    );let definitions
    (if (equal tempList ())
      ;then
      nil
      ;else
      (first tempList)
    );if
  );let
);is-point-in-ignore-list

;; Tests:
(ert-deftest is-point-in-ignore-list-test ()
"This function tests the helper function is-point-in-ignore-list.
The test-cases are the following:
1. ignore list empty
2. point not in ignore list, while ignore list is not empty.
3. point in ignore list
4. Point in ignore-list boundary case left
5. Point in ignore-list boundary case right"
  ;1.
  (should (equal (is-point-in-ignore-list 3 nil) nil))
  ;2.
  (should (equal (is-point-in-ignore-list 3 '((5 6) (7 15) (20 75)))
		 nil))
  ;3.
  (should (equal (is-point-in-ignore-list 10 '((5 6) (7 15) (20 75)))
		 '(7 15)))
  ;4.
  (should (equal (is-point-in-ignore-list 20 '((5 6) (7 15) (20 75)))
		 '(20 75)))
  ;5.
  (should (equal (is-point-in-ignore-list 6 '((5 6) (7 15) (20 75)))
		 '(5 6)))
);is-point-in-ignore-list-test


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
 - The point p is at the beginning of a word
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
     (temp-touple ())
     (result "")
     (temp-word "")
    );let definitions
    (while (and (> i 0) flag)
      (setq curpos (point))
      (re-search-forward "[[:space:]\n]+" (point-max) t)
      (if (equal curpos (point))
	  (setq flag nil)
      );if
      (setq temp-word "")
      (while (< curpos (point))
	(if (not (is-point-in-ignore-list curpos ign))
	    (setq temp-word (concat temp-word
				    (buffer-substring-no-properties
				     curpos (+ 1 curpos))))
	);if
	(setq curpos (+ 1 curpos))
      );while
      (if (string-match "[[:alpha:]]" temp-word)
	  (progn
	    (setq result (concat result temp-word))
	    (setq i (- i 1))
	  );progn
      );if
    );while
    ;; (setq curpos (point))
    (goto-char p)
    (if flag
    	result
        ""
    );if
  );let
);;get-next-n-words-with-ignore-list

;; Tests:
(ert-deftest get-next-n-words-with-ignore-list-test ()
"Here, we test the function get-next-n-words with-ignore-list. The
covered test cases are:
1. empty file, empty ignore list.
2. empty file, non-empty ignore list.
3. non-empty file, empty ignore list.
4. non-empty file, non-empty ignore-list border case producing a
   non-empty string.
5. non-empty file, non-empty ingnore-list border case producing an
   empty string.
6. non-empty file, non-empty ignore-list non-border case producing a
   non-empty string.
7. non-empty file, non-empty ignore-list non-border case producing an
   empty string.
8. Whole buffer is ignored."
  ;1.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 nil) ""))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 '((0 50) (70 100))) ""))
  (kill-buffer "empty_test_buffer.txt")
  ;3.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 nil) ""))
  (kill-buffer "test_buffer_3_words.txt")
  ;4.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 2 1 '((1 6)))
		 "ipsum dolor.\n"))
  (kill-buffer "test_buffer_3_words.txt")
  ;5.
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 3 1 '((13 18)))
		 ""))
  (kill-buffer "test_buffer_3_words.txt")
  ;6.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 10 1 
						    '((1 6) (13 18)
						      (117 136)))
		 "ipsum sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod "))
  (kill-buffer "test_buffer_50_words.txt")
  ;7.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 
						    '((1 6) (13 18)
						      (117 136)))
		 ""))
  (kill-buffer "test_buffer_50_words.txt")
  ;8.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 20 1 
						    '((1 296)))
		 ""))
  (kill-buffer "test_buffer_50_words.txt")
);get-next-n-words-with-ignore-list-test

;(get-next-n-words-with-ignore-list 100 1 '((1 2000) (3000 4000)))

(defun exceeders (str number)
"string->integer->listof (list string integer)
Given a string str, and a number 'number', this function returns a
list of tuples (a, b), where a is a word in str, which appears b
times in str, where b>=number. Furthermore, for a, we ignore it
if its length is smaller than 4 letters, and we remove all space
characters, as well as digits and punctuation symbols, and all the
letters in a are lowercase.
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

;; Tests

(ert-deftest exceeders-test ()
"This function tests the exceeders-function.
The covered test-cases are:
1. Empty string
2. number is 0, and all the words in the list have 4 or more
   characters and do not contain special symbols.
3. number is 0, and all words in the list have 4 or more characters,
   and contain special symbols.
4. number is 0, and there is no whitespace in the string. Only
   characters, symbols and numbers.
5. number is 0, and there are some less than four letter words in the
   string.
6. number is not 0, and no word is equal or exceeds the number.
7. number is not 0, and some, but not all, words do exceed this number."
  ;1.
  (should (equal (exceeders "" 2) nil))
  ;2.
  (should (equal (exceeders "test test test test Albert" 0)
		 (list (list "test" 4) (list "albert" 1))))
  ;3.
  (should (equal (exceeders "test. test! test? Albert1" 0)
		 (list (list "test" 3) (list "albert" 1))))
  ;4.
  (should (equal (exceeders "test.test!2353%%^_test????123Albert1" 0)
		 (list (list "test" 3) (list "albert" 1))))
  ;5.
  (should (equal (exceeders "abc test 1234 hallo wo bist du?" 0)
		 (list (list "test" 1)
		       (list "hallo" 1)
		       (list "bist" 1))))
  ;6.
  (should (equal (exceeders "test test test test test. This is a \
normal normal sentence" 6) nil))
  ;7.
  (should (equal (exceeders "test abc test abc test. hallo hallo." 3)
		 (list (list "test" 3))))
);exceeders-test

(provide 'repetition-error)
;;; repetition-error.el ends here
