Repetition Error finding in Emacs (BETA)
========================================

This Emacs package helps people who write longer texts to check
whether or not they have some repetition errors in there. With
repetition errors, we do not just mean those of the form where the
same word is repeated repeated right after itself, but those where you
use the same word twice in close proximity. After you detect such an
error, you can either leave it as is, or consult a thesaurus to find
an alternative word to use.


How to Install it
-----------------

I personally have the file `repetition-error.el` located in a
subfolder of my `.emacs.d` folder. In my `.emacs`-file, I simply added
the line:

~~~~
(load "~/.emacs.d/your_subfolder/repetition-error.el")
~~~~

Execute the line (or restart Emacs), and you are ready to go.

Optionally, one can also test the package to make sure all commands
work properly on the installed Emacs version. This can be done by
opening the `repetition-error.el` package and run
`ert-run-tests-interactively`. If there is no error, then the package
runs as expected on your system.


How to Use it
-------------

1. Write your text
2. Use `M-x-find-reperr-whole-buffer` to find all repetition errors in
   the current document. Emacs will switch into an interactive mode,
   which highlights each finding of a repetition error, and shows the
   next one when pressing `c` (continue). This interactive mode can be
   left -- e.g. in order to correct a found valid repetition error --
   by pressing any key but `c`. Afterwards, one can continue the
   search from the current point on by using
   `M-x-find-reperr-from-point` (this can also be used from the start,
   if one is only interested in all repetitions after a certain
   point).
3. When the search for repetition errors is finished, the message
   "Finished finding repetition errors" is displayed in the
   minibuffer.

**REMARK**: There is also a command called
`M-x find-reperr-latex-whole-buffer` resp.
`M-x find-reperr-latex-from-point`, which ignores certain LaTeX
regions in the search (e.g. comments, math modes, etc.). This feature
is very naively implemented, and might not be able to exclude all
possible LaTeX-commands out there. However, taking its outputs with a
grain of salt makes it a useful tool (I personally used it for my
Ph.D. thesis).


How to Extend/Experiment with it
--------------------------------

There are certain parameters which can be set differently, depending
on how the user wants it.

 - `repetition-error-word-block-size`: This variable stores the word
   block size, in which the user does not want any
   repetitions. The default value is 100.
 - `repetition-error-min-occurrence`: This variable stores how often a
   value needs to occur in a word-block at least to be detected as a
   repetition error. The default value is 2.
 - `min-not-ignore-letters`: Some words like "and" will inevitably
   occur more than once in a word block, and we would like to ignore
   these. Hence, we introduce this variable to store the minimum
   length of the word to be considered for a repetition error.  Its
   standard value is 4. This means, that words like and, it, ... will
   be ignored in the search for repetitions, but words like then,
   than, they will not.

Other than that, as mentioned above, there is a way to customize the
script to ignore commands or other word blocks, given you are writing
in a WYSIWYM language like LaTeX. If you want to customize for your
language, please study the code and see how this was achieved for the
LaTeX part. It is fairly modular, but it requires to study the code in
a bit more detail.
