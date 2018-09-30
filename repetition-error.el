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
;; The main function in this package is "find-reperr". It
;; takes a starting point and an end point, and if the section is long
;; enough (i.e. enough words given), it will highlight repetitions,
;; and ask the user either to continue and ignore that repetition, or
;; to quit the search. In the latter case, the current cursor remains
;; at the position, and the user can correct the error, if he or she
;; wants.
;; There are the following functions that can be interactively
;; called.
;; find-reperr-whole-buffer:
;;   Runs the function "find-reperr" from the beginning
;;   until the end of the whole buffer
;; find-reperr-from-point:
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
;;   - Experiment with different block sizes and repetition
;;     occurrences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defvar min-not-ignore-letters 4
"
  (Positive Integer)
This variable stores the minimum word length, for which the word will
not be ignored in the search for repetitions. Its standard value is
4. This means, that words like and, it, ... will be ignored in the
search for repetitions, but words like then, than, they will not.
"
);min-not-ignore-letters

(defvar rep-temp-word-block nil
"
If not nil, this is a three touple (s i j) consisting of a string s,
two non-negative integers i and j with i<j, which represent
the starting position of s in the buffer and the ending position
(remark: j is not necessarily i+length(s), because we might have
an ignore list going around)
"
)

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

(byte-compile 'transform-complete-ci-string)

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


(defun find-reperr-whole-buffer ()
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
  (find-reperr (point-min) (point-max) repetition-error-word-block-size repetition-error-min-occurrence)
);find-reperr-whole-buffer


(defun find-reperr-latex-whole-buffer ()
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
  (find-reperr (point-min)
			 (point-max)
			 repetition-error-word-block-size
			 repetition-error-min-occurrence
			 (create-ignore-list-for-latex-buffer))
);find-reperr-latex-whole-buffer

(defun find-reperr-from-point ()
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
  (find-reperr (point) (point-max) repetition-error-word-block-size repetition-error-min-occurrence)
);find-reperr-whole-buffer

(defun find-reperr-latex-from-point ()
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
  (find-reperr (point)
			 (point-max)
			 repetition-error-word-block-size
			 repetition-error-min-occurrence
			 (create-ignore-list-for-latex-buffer))
);find-reperr-latex-whole-buffer

(defun find-reperr (begin end &optional nWords minRep ignlist)
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
    (setq rep-temp-word-block nil)
    (recenter 0)
    (let
	(;let definitions
	 (flag t)
	 (curWordBlock nil)
	 (exc nil)
	 (tempExc nil)
	 (tempKnownsList nil)
	 (usrcmd nil)
	 (temp-touple nil)
	 (temp-pos 0)
	);let definitions
      (while flag
	(if (not ignlist)
	    (setq curWordBlock (get-next-n-words-from-point nWords (point)))
	    ;else
	  ;(setq ignlist (cl-remove-if (lambda (x) (< (nth 1 x) (point))) ignlist))
	  (while (and
		  (not (equal ignlist nil))
		  (< (nth 1 (nth 0 ignlist)) (point)))
	    (setq ignlist (cdr ignlist))
	  );while
	  (setq curWordBlock (get-next-n-words-with-ignore-list nWords (point) ignlist))
	);if
	(if
	  (>= (nth 2 curWordBlock) end)
	  (progn
	    (setq flag nil)
	    "Reached the end of the buffer"
	  );progn for then
		;else
	  (setq exc (filter-known-words tempKnownsList
					(exceeders
					 (nth 0 curWordBlock)
					 minRep)
					(nth 1 curWordBlock)
					(nth 2 curWordBlock)))
	  (setq tempKnownsList
		(update-knowns-list tempKnownsList exc
				    (nth 1 curWordBlock)
				    (nth 2 curWordBlock)))
	  (if (equal exc ())
	      (progn
		(re-search-forward "[[:space:]\n]+" end t)
		(recenter 0)
		(if (> (point) end)
		  (setq flag nil)
		);if
		(if ignlist
		  ;;in this case, we can move even further
		  (setq temp-touple (is-point-in-ignore-list (point) ignlist))
		  (while (and
			  (not (equal temp-touple nil))
			  (<= (nth 1 temp-touple) end))
		    ;(progn

		      ;(setq ignlist (cl-remove-if (lambda (x) (< (nth 1 x) (point))) ignlist))
		      (goto-char (+ 1 (nth 1 (is-point-in-ignore-list
					  (point) ignlist))))
		      (recenter 0)
		      (setq temp-touple (is-point-in-ignore-list
					 (point) ignlist))
		      (if (or
			   (> (point) end)
			   (and
			    (not (equal temp-touple nil))
			    (>= (nth 1 temp-touple) end)))
		        (setq flag nil)
		      );if
		    ;);progn
		  );while
		);if
	      );progn
		;else
	    (setq tempExc exc)
	    (while (not (equal tempExc ()))
	      (setq temp-pos (point))
	      (re-search-forward (transform-complete-ci-string (car
		      (car tempExc))) end t)
	      (recenter 0)
	      (highlight-regexp (transform-complete-ci-string (car (car tempExc))))
	      (setq usrcmd
		    (read-char (format "Repeated word: \"%s\". (c) Continue search for repetition errors or (any key) quit?" (car (car tempExc))))
	      );setq
	      (unhighlight-regexp (transform-complete-ci-string (car
		      (car tempExc))))
	      (goto-char temp-pos)
	      (recenter 0)
	      (setq tempExc (cdr tempExc))
	      (if (not (equal usrcmd 99));;99 is ASCII for 'c'
                (progn
		  (setq flag nil)
		  (setq tempExc ())
		);progn
	      );if
	    );while
	    (re-search-forward "[[:space:]\n]+" end t)
	    (recenter 0)
	    (if (> (point) end)
	      (setq flag nil)
	    );if
	    (if ignlist
	      ;;in this case, we can move even further
	      (setq temp-touple (is-point-in-ignore-list (point) ignlist))
	      (while (and
			  (not (equal temp-touple nil))
			  (<= (nth 1 temp-touple) end))
	        ;(progn
	    	  ;(setq ignlist (cl-remove-if (lambda (x) (< (nth 1 x) (point))) ignlist))
	          (goto-char (+ 1 (nth 1 (is-point-in-ignore-list (point) ignlist))))
	          (recenter 0)
		  (setq temp-touple (is-point-in-ignore-list
				     (point) ignlist))
		  (if (or
		       (> (point) end)
		       (and
			(not (equal temp-touple nil))
			(>= (nth 1 temp-touple) end)))
		      (setq flag nil)
		    );if
	        ;);progn
	      );while
	    );if
	  );if
	);if
      );while
    );let
  );if
  (message "Finished finding repetition errors")
  ;);save-excursion
);find-reperr

(byte-compile 'find-reperr)

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
       (result (reverse (cl-remove-if (lambda (m) (if (< (nth 2 m) leftBound) t nil)) knownList)))
      );let definitions
    (while (not (equal tempNE ()))
      (setq result
	    (cons (cons (nth 0 (nth 0 tempNE)) (cons leftBound (cons rightBound())))
		  result))
      (setq tempNE (cdr tempNE))
    );while
    (reverse result)
  );let
);update-knowns-list

(byte-compile 'update-knowns-list)

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
  (cl-labels
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
		   (equal (nth 0 (nth 0 tempKL)) (nth 0 el))
		   (< (nth 1 (nth 0 tempKL)) rightBound)
		   (> (nth 2 (nth 0 tempKL)) leftBound)
	        );and
		(setq flag t)
	      );if
	      (setq tempKL (cdr tempKL))
	    );while
	    flag
	  );let
       );compareToKnownList
      );labels definitions
    (cl-remove-if (lambda (el) (compareToKnownList knownList el)) newExceeders)
  );labels
);filter-known-words

;(byte-compile 'filter-known-words)
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
"Integer->Integer->(list string Integer Integer)
Given an integer n and an integer p. The parameter p represents a
position in the buffer, n represents a number of words we want to
extract. This function returns a three tuple, containing:
- a string containing the next n words from point p in the buffer, if
  available. If there are no n words, then the function returns what
  is available.
- p itself
- the position when this string ends in the buffer
ASSUMPTIONS:
 - The point p is at the beginning of a word
 - if the variable rep-temp-word-block is set,
   p will be ignored and we assume that p is
   (nth 1 rep-temp-word-block)
SIDE EFFECTS:
 - The cursor will in the end actually be moved to position p
 - Accesses cursor positions
"
  (goto-char p)
  (if (not rep-temp-word-block)
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
	(setq rep-temp-word-block
					;(if flag
	      (list (buffer-substring-no-properties p curpos) p curpos))
					;(list "" p curpos)
					;);if
	rep-temp-word-block
	);let
    ;else
    (goto-char (nth 1 rep-temp-word-block))
    (let
	(;let definitions
	 (curWordBlock (nth 0 rep-temp-word-block))
	 (begin-pos (nth 1 rep-temp-word-block))
	 (end-pos (nth 2 rep-temp-word-block))
	);let definitions
      (re-search-forward "[[:space:]\n]+" (point-max) t)
      (setq begin-pos (point))
      (goto-char end-pos)
      (re-search-forward "[[:space:]\n]+" (point-max) t)
      (setq end-pos (point))
      (setq rep-temp-word-block
	    (list (buffer-substring-no-properties begin-pos end-pos)
		  begin-pos end-pos))
      (goto-char begin-pos)
      rep-temp-word-block
    );let
  );if
);;get-next-n-words-from-point

(byte-compile 'get-next-n-words-from-point)

(ert-deftest get-next-n-words-from-point-test ()
"Here, we test the function get-next-n-words-from-point.
Our test suite contains the following test-cases:
NO set rep-temp-word-block
1. An empty buffer
2. Boundary case for number of words with boundary that has the exact
   number of words available
3. Boundary case for number of words with boundary that goes over the
   number of available words.
4. Large text, boundary case with exact number of words.
5. Large text, boundary case with more words asked for than available.
6. Large text, non-boundary case with way more words asked for than available.
7. Large text, non-boundary case producing text.
SET rep-temp-word-block
8. Boundary case: moving one word forward in a block where there is
                  only one more word.
"
  ;1.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-from-point 100 1) (list "" 1 1)))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-from-point 3 1) (list "Lorem ipsum \
dolor.\n" 1 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;3.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-from-point 4 1) (list "Lorem ipsum \
dolor.\n" 1 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;4.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 50 1) (list "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
sed diam voluptua. At vero eos et accusam et justo duo dolores et ea
rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem
ipsum dolor sit amet.
" 1 297)))
  (kill-buffer "test_buffer_50_words.txt")
  ;5.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 51 1) (list "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
sed diam voluptua. At vero eos et accusam et justo duo dolores et ea
rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem
ipsum dolor sit amet.
" 1 297)))
  (kill-buffer "test_buffer_50_words.txt")
  ;6.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 100 1) (list "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
sed diam voluptua. At vero eos et accusam et justo duo dolores et ea
rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem
ipsum dolor sit amet.
" 1 297)))
  (kill-buffer "test_buffer_50_words.txt")
  ;7.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-from-point 3 1) (list "Lorem ipsum \
dolor " 1 19)))
  (kill-buffer "test_buffer_50_words.txt")
  ;8
  (setq rep-temp-word-block (list "Lorem ipsum " 1 13))
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-from-point 2 1) (list "ipsum \
dolor.\n" 7 20)))
  (kill-buffer "test_buffer_3_words.txt")
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

(byte-compile 'create-ignore-list-by-regexp)

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
our find-reperr routines, assuming that the current document
is a LaTeX file. In particular, this function will detect matches to
the following expressions and ignore them:
- Math-modes (\[.*\], \(.*\), $.*$, $$.*$$)
- \begin{.+} and \end{.+}
- \[a-zA-Z0-9]+[{]? in general (commands)
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
    ;;comments
    (setq result (append result (create-ignore-list-by-regexp
				 "%.*?$")))
    ;;begin/end eqnarray
    (setq result (append result (cl-remove-if
				 (lambda (x)
				   (is-point-in-ignore-list
				    (nth 0 x) result))
				 (create-ignore-list-by-regexp
				  "[\\]begin{eqnarray[\*]?}\\(.\\|\n\\)+?[\\]end{eqnarray[\*]?}"))))
    ;;begin/end align
    (setq result (append result (cl-remove-if
				 (lambda (x)
				   (is-point-in-ignore-list
				    (nth 0 x) result))
				 (create-ignore-list-by-regexp
				  "[\\]begin{align[\*]?}\\(.\\|\n\\)+?[\\]end{align[\*]?}"))))
    ;;math notations a la \( ... \)
    (setq result (append result (create-ignore-list-by-regexp
				 "[\\][(]\\(.\\|\n\\)+?[\\][)]")))
    ;;math notations a la \[ ... \]
    (setq result (append result
			 (mapcar
			  (lambda (m) (cons (+ 1 (nth 0 m)) (cons
							      (nth 1
							       m) '())))
			  (create-ignore-list-by-regexp
			   "\\([^\\]\\|^\\)[\\]\\[\\(.\\|\n\\)+?[\\]\\]"))))
    ;;In the line before: We needed to remove the case \\[12pt] e.g.,
    ;;which is covered by the next case.
    (setq result (append result (create-ignore-list-by-regexp
				 "[\\][\\]\\[[0-9]+[[:alpha:]]*?\\]")))
    (while (< curpos (point-max))
      (setq foundFlag nil)
      (if (and
	    (equal (string (char-after curpos)) "$")
	    (not (is-point-in-ignore-list curpos result))
	  );;and
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
    ;;begin{...} in general
    (setq result (append result (cl-remove-if
				 (lambda (x)
				   (is-point-in-ignore-list
				    (nth 0 x) result)) (create-ignore-list-by-regexp
					       "[\\]begin{.+?}"))))
    ;;end{...} in general
    (setq result (append result (cl-remove-if
				 (lambda (x)
				   (is-point-in-ignore-list
				    (nth 0 x) result)) (create-ignore-list-by-regexp
					       "[\\]end{.+?}"))))
    (setq result (append result (cl-remove-if
				 (lambda (x)
				   (is-point-in-ignore-list
				    (nth 0 x) result)) (create-ignore-list-by-regexp
					       "[\\]\\([[:alnum:]]\\|\\[\\|\\]\\|\\)+"))))
    (sort result (lambda (x y) (<= (nth 0 x) (nth 0 y))))
  );let
);create-ignore-list-for-latex-buffer ()

(byte-compile 'create-ignore-list-for-latex-buffer)

;; Tests
(ert-deftest create-ignore-list-for-latex-buffer-test ()
"Here, we test the function create-ignore-list-for-latex-buffer.
The test cases are the following:
1. empty buffer.
2. Buffer with no LaTeX in it.
3. Valid LaTeX-Buffer, containing all the ignored LaTeX constructs."
  ;1.
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (create-ignore-list-for-latex-buffer) nil))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (create-ignore-list-for-latex-buffer) nil))
  (kill-buffer "test_buffer_50_words.txt")
  ;3.
  (set-buffer (find-file "./test_files/latex_test_file.tex"))
  (should (equal (create-ignore-list-for-latex-buffer)
		 '((1 66) (67 134) (135 197) (198 261) (262 328)
		   (329 391) (392 458) (459 521) (522 580) (581 647)
		   (648 659) (661 724) (726 787) (809 872) (912 926)
		   (1008 1018) (1103 1144) (1178 1199) (1199 1207)
		   (1238 1250) (1252 1267) (1268 1273) (1283 1288)
		   (1298 1303) (1313 1318) (1328 1333) (1343 1348)
		   (1358 1371) (1373 1381) (1408 1418))))
  (kill-buffer "latex_test_file.tex")
);create-ignore-list-for-latex-buffer-test

(defun is-point-in-ignore-list (p ign)
"Integer->listof (Integer Integer)->(Integer Integer)
Given an integer p, and a list of integer tuples ign.
If for a tuple (i j) in p we have that i<=p<=j, the function returns
the first interval in ign with p in it, and nil otherwise.
"
  (let
    (;let definitions
      (tempList (cl-remove-if-not (lambda (m) (and (<= (nth 0 m) p) (<= p (nth 1 m)))) ign))
    );let definitions
    (if (equal tempList ())
      ;then
      nil
      ;else
      (nth 0 tempList)
    );if
  );let
);is-point-in-ignore-list

(byte-compile 'is-point-in-ignore-list)

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
"Integer->Integer->listof (list int int)->(list string Int Int)
Given an integer n, an integer p, and a list of integer tuples ign.
The parameter p represents a
position in the buffer, n represents a number of words we want to
extract. This function returns a tuple containing
- a string containing the next n words from point p in the buffer, if
  available. If for a tuple (i j) in ignore, a word appears at
  position somewhere between i and j, it will be ignored.
  If there are no n words, then there will be the empty string here
- the point p
- and the position where the string ended
ASSUMPTIONS:
 - The point p is at the beginning of a word
 - The beginning of a regexp is found after p, not before.
SIDE EFFECTS:
 - The cursor will in the end actually be moved to position p
 - Accesses cursor positions
"
  (goto-char p)
  (if (not rep-temp-word-block)
      (let
	  (;let definitions
	   (flag t)
	   (i n)
	   (curpos (point))
	   (temp-touple ())
	   (result "")
	   (temp-word "")
	   (temp-begin 0)
	   );let definitions
	(while (and (> i 0) flag)
	  (setq temp-touple (is-point-in-ignore-list (point) ign))
	  (while (and
		  (not (equal temp-touple ()))
		  (<= (nth 1 temp-touple) (point-max))
		  )
	    (goto-char (+ 1 (nth 1 temp-touple)))
	    (setq temp-touple (is-point-in-ignore-list (point) ign))
	    );while
	  (setq curpos (point))
	  (re-search-forward "[[:space:]\n]+" (point-max) t)
	  (if (>= curpos (point))
	      (setq flag nil)
	    );if
	  (setq temp-word "")
	  (while (and
		  (< curpos (point))
		  (not (is-point-in-ignore-list curpos ign)))
	    (setq temp-word (concat temp-word
				    (buffer-substring-no-properties
				     curpos (+ 1 curpos))))
	    (setq curpos (+ 1 curpos))
	    );while
	  (if (string-match "[[:alpha:]]" temp-word)
	      (progn
		(setq result (concat result temp-word))
		(setq i (- i 1))
		);progn
	    );if
	  );while
	(setq curpos (point))
	(goto-char p)
	(setq temp-begin (point))
	(setq temp-touple (is-point-in-ignore-list temp-begin ign))
	(while (and temp-touple
		    (< temp-begin (point-max)))
	  (setq temp-begin (+ 1 (nth 1 temp-touple)))
	  (setq temp-touple (is-point-in-ignore-list temp-begin ign))
	);while
	(setq rep-temp-word-block (list result temp-begin curpos))
					;(if flag
    	rep-temp-word-block
					;    (list "" p curpos)
					;);if
	);let
    ;else
    (goto-char (nth 1 rep-temp-word-block))
    (let
	(;let definitions
	 (begin-pos (nth 1 rep-temp-word-block))
	 (end-pos (nth 2 rep-temp-word-block))
	 (curWordBlock (nth 0 rep-temp-word-block))
	 (temp-touple nil)
	 (temp-pos 0)
	 (flag t)
	 (temp-word "")
	);let definitions
        ;; We begin by deleting the first word in curWordBlock
        (goto-char begin-pos)
	(re-search-forward "[[:space:]\n]+" (point-max) t)
	(setq curWordBlock (substring curWordBlock (- (point)
						      begin-pos)))
	(setq begin-pos (point))
	(setq temp-touple (is-point-in-ignore-list begin-pos ign))
	(while temp-touple
	  (setq begin-pos (+ 1 (nth 1 temp-touple)))
	  (setq temp-touple (is-point-in-ignore-list begin-pos ign))
	);while
	;; now we find the next word at the end.
	(goto-char end-pos)
	(while flag
	  (setq temp-touple (is-point-in-ignore-list (point) ign))
	  (while (and
		  (not (equal temp-touple ()))
		  (<= (nth 1 temp-touple) (point-max))
		  )
	    (goto-char (+ 1 (nth 1 temp-touple)))
	    (setq temp-touple (is-point-in-ignore-list (point) ign))
	  );while
	  (setq temp-pos (point))
	  (re-search-forward "[[:space:]\n]+" (point-max) t)
	  (if (>= temp-pos (point))
	      (setq flag nil)
	    );if
	  (setq temp-word "")
	  (while (and
		  (< temp-pos (point))
		  (not (is-point-in-ignore-list temp-pos ign)))
	    (setq temp-word (concat temp-word
				    (buffer-substring-no-properties
				     temp-pos (+ 1 temp-pos))))
	    (setq temp-pos (+ 1 temp-pos))
	    );while
	    (if (string-match "[[:alpha:]]" temp-word)
	      (progn
		(setq curWordBlock (concat curWordBlock temp-word))
		(setq flag nil)
	      );progn
	    );if
	);while
	(setq end-pos (point))
	(setq rep-temp-word-block (list curWordBlock begin-pos
					end-pos))
	(goto-char begin-pos)
	rep-temp-word-block
    );let
  );if
);;get-next-n-words-with-ignore-list

(byte-compile 'get-next-n-words-with-ignore-list)

;; Tests:
(ert-deftest get-next-n-words-with-ignore-list-test ()
"Here, we test the function get-next-n-words with-ignore-list. The
covered test cases are:
TESTS WITHOUT rep-temp-word-block
1. empty file, empty ignore list.
2. empty file, non-empty ignore list.
3. non-empty file, empty ignore list.
4. non-empty file, non-empty ignore-list border case with exactly the
   number of words we are asking for.
5. non-empty file, non-empty ingnore-list border case with one more
   word asked for than available.
6. non-empty file, non-empty ignore-list non-border case having more
   words available than asked for.
7. non-empty file, non-empty ignore-list non-border case asking for
   more words than available.
8. Whole buffer is ignored.
TESTS WITH rep-temp-word-block
9. Non-empty ignore list, non-empty-text, next word.
10. Non-base-example, similar to 6
"
  ;1.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 nil) (list ""
								    1 1)))
  (kill-buffer "empty_test_buffer.txt")
  ;2.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/empty_test_buffer.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 '((0 50) (70
								   100)))
		 (list "" 1 1)))
  (kill-buffer "empty_test_buffer.txt")
  ;3.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1 nil)
		 (list "Lorem ipsum dolor.\n" 1 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;4.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 2 1 '((1 6)))
		 (list "ipsum dolor.\n" 7 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;5.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 3 1 '((13 18)))
		 (list "Lorem ipsum " 1 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;6.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 10 1
						    '((1 6) (13 18)
						      (117 136)))
		 (list "ipsum sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod " 7 81)))
  (kill-buffer "test_buffer_50_words.txt")
  ;7.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 50 1
						    '((1 6) (13 18)
						      (117 136)))
		 (list "ipsum sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod tempor invidunt ut labore et dolore sed diam voluptua. At vero eos et accusam et justo duo dolores et ea\nrebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem\nipsum dolor sit amet.\n" 7 297)))
  (kill-buffer "test_buffer_50_words.txt")
  ;8.
  (setq rep-temp-word-block nil)
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 20 1
						    '((1 296)))
		 (list "" 297 297)))
  (kill-buffer "test_buffer_50_words.txt")
  ;9.
  (setq rep-temp-word-block (list "ipsum " 7 13))
  (set-buffer (find-file "./test_files/test_buffer_3_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 1 1 '((1 6)))
		 (list "dolor.\n" 13 20)))
  (kill-buffer "test_buffer_3_words.txt")
  ;10
  (setq rep-temp-word-block (list "ipsum sit amet, consetetur sadipscing elitr, sed diam
nonumy " 1 74))
  (set-buffer (find-file "./test_files/test_buffer_50_words.txt"))
  (should (equal (get-next-n-words-with-ignore-list 9 1
						    '((1 6) (13 18)
						      (117 136)))
		 (list "sit amet, consetetur sadipscing elitr, sed diam
nonumy eirmod " 7 81)))
  (kill-buffer "test_buffer_50_words.txt")
);get-next-n-words-with-ignore-list-test

(defun count-if (fun lst)
"function(element):boolean -> list(element) -> integer
Given a predicate function func and a list lst, this function counts
the number of elements e in the lst for which f(e) = true
"
  (let ((value 0))
    (dolist (elt lst value)
      (if (funcall fun elt)
          (setq value (+ 1 value))))))

(byte-compile 'count-if)

(ert-deftest count-if ()
"Here are the tests covered:
1. Trivial function empty list
2. Trivial function non-empty list
3. non-trivial function empty list
4. non-trivial function non-empty list
5. false function non-empty list"
;1
  (setq temp-lst nil)
  (setq true-fun (lambda (x) t))
  (should (equal (count-if true-fun temp-lst) 0))
;2
  (setq temp-lst (list 1 2 3))
  (should (equal (count-if true-fun temp-lst) 3))
;3
  (setq temp-lst nil)
  (setq non-trivial-function (lambda (x) (< x 5)))
  (should (equal (count-if non-trivial-function temp-lst) 0))
;4
  (setq temp-lst (list 1 2 3 4 7 8 9 10))
  (should (equal (count-if non-trivial-function temp-lst) 4))
;5
  (setq false-fun (lambda (x) nil))
  (should (equal (count-if false-fun temp-lst) 0)))

(defun exceeders (str number)
"string->integer->listof (list string integer)
Given a string str, and a number 'number', this function returns a
list of tuples (a, b), where a is a word in str, which appears b
times in str, where b>=number. Furthermore, for a, we ignore it
if its length is smaller than min-not-ignore-letters letters, and we remove all space
characters, as well as digits and punctuation symbols, and all the
letters in a are lowercase.
"
  (cl-labels
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
	     (setq tempList (cl-remove-if (lambda (m) (equal (downcase m) (downcase (car tempList)))) tempList))
	   );while
	   (nreverse result)
	 );let
	);count-each-word
       );;function definitions
    (;labels body
     let
      (;let definitions
       (filtered-inp (cl-remove-if (lambda (m) (< (string-width m) min-not-ignore-letters))
				(split-string str
					      "[[:punct:][:digit:][:space:]\n]"
					      t)))
      );;let definitions
      (;let body
       cl-remove-if (lambda (k) (< (nth 1 k) number)) (count-each-word
						     filtered-inp)
      );let body
    );labels body
  );;labels
);exceeders

;(byte-compile 'exceeders)
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
