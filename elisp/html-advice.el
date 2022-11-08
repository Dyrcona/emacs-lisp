;; ---------------------------------------------------------------
;; Copyright © 2022 Jason J.A. Stephenson <jason@sigio.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; ---------------------------------------------------------------

;; html-advice.el:

;; My advice and functions for working with html text in GNU Emacs'
;; html-mode.  I may, one day soon, just write my own html-mode.

;; Constants and variables used in the code below.
;; I put these at the top to keep the compiler quiet.

(defconst blank-line "^\\s *$" "Regexp to match line that's either
empty or all whitespace.")

;; My functions and commands (interactive functions) for changing how
;; html-mode commands work or adding functionality. These also come
;; before the advice functions that call them to keep the compiler
;; quiet.

(defun make-toc ()
  "Makes a table of contents block for the current buffer.

This function searches the current buffer for headline (<hN>) tags
that contain named anchor tags and extracts the headline level
(i.e. the numeric portion of the <hN>), the anchor's named target
(i.e. the value of the name attribute), and the anchor text (i.e. what
is enclosed between the <a> and </a>.

Once that list is gathered, the function then runs back throught the
buffer looking for a line composed entirely of the text <!-- Begin TOC
-->. After finding that it stores a mark and searches forward for
another line containing only <!-- End TOC -->. (Should either search
fail, the function ceases running and the buffer remains unchanged.)
Another mark is saved and kill-region is called to empty the buffer
between the two matched lines.

Now, to write out the actual table of contents, the function loops
through the list and extracts the needed data. The very first headline
level is used as a base and is assumed to have the lowest numeric
value in the buffer. If not, then the function may just blow up. As
the code cycles through the list, it opens a new paragraph any time it
encounters a headline level that is equal to the base headline
level. If a previous paragraph was created and needs closing, then the
closing paragraph tag, </p>, is also written to the buffer.

When a headline level that is not equal to the base is encountered, a
line break tag is inserted, followed by span with a class attribute
whose values is the string \"indent\" with the difference between the
current level and the base level tacked on. This gives a possible
value from indent1 to indent5. It is assumed that you are using a
stylesheet to indent these lines an appropriate distance. In my case,
I have a stylesheet where each indentN indents N * 0.25in.

Once all is done, a final </p> is written out, and the cursor is
returned to wherever in the buffer the command was originally run. The
comments that delineate where the TOC is to be inserted are left
intact by the above process, so the command may be run as often as
needed on a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((toc-data '()))
      (while (re-search-forward
              "<h\\([1-6]\\).*><a name=\"\\(.+\\)\">\\(.+\\)</a></h\\1>"
              nil t)
        (setq toc-data (cons (list (match-string 1) (match-string 2)
                                   (match-string 3)) toc-data)))
      (setq toc-data (reverse toc-data))
      (goto-char (point-min))
      (let ((start nil) (end nil))
        (re-search-forward "^<!-- Begin TOC -->$")
        (end-of-line)
        (forward-char 1)
        (setq start (point))
        (re-search-forward "^<!-- End TOC -->$")
        (beginning-of-line)
        (setq end (point))
        (kill-region start end))
      (let ((level nil) (prev nil) (base nil) (sub-toc '())
            (target nil) (name nil))
        (while (consp toc-data)
          (setq sub-toc (car toc-data)
                toc-data (cdr toc-data)
                target (car (cdr sub-toc))
                name (replace-regexp-in-string "\\s +" "&nbsp;"
                                               (car (cdr (cdr sub-toc)))))
          (if (null base)
                (setq base (string-to-number (car sub-toc))
                      level base)
            (setq level (string-to-number (car sub-toc))))
          (if (= level base)
              (progn
                (if (not (null prev))
                    (insert "</p>\n"))
                (insert "<p>\n")
                (insert (format "<a href=\"#%s\">%s</a>\n" target name)))
            (insert (format "<br>\n<span class=\"indent%d\">" (- level base)))
            (insert (format "<a href=\"#%s\">%s</a></span>\n" target name)))
          (setq prev level))
        (insert "</p>\n")))))

(defun my-html-list (which)
  "A generic replacement for html-ordered-list and
html-unordered-list. This function is meant to be called in the advice
for those two commands. (Notice that it isn't interactive.) It takes a
single string argument of either ul or ol depending on whether you
want an unordered or ordered list, respectively. (Errm, it actually
doesn't check its argument so whatever string you give it gets
inserted in the output, but it's an internal function and you won't
abuse it, will you?)"
  (beginning-of-line)
  (if (looking-at blank-line)
      (progn
        (insert (format "<%s>\n<li></li>\n</%s>\n" which which))
        (re-search-backward "</li>" nil t))
    (progn
      (if (re-search-backward blank-line nil t)
          (progn
            (insert (format "\n<%s>" which))
            (forward-char 1))
        (progn
          (goto-char (point-min))
          (insert (format "<%s>\n" which))))
      (while (not (looking-at blank-line))
        (html-list-item)
        (forward-line))
      (insert (format "</%s>\n" which)))))

(defun my-start-line ()
  "Goes to the beginning of the line and removes any leading spaces,
if the line is not empty."
  (beginning-of-line)
  (if (not (looking-at blank-line))
      (while (looking-at "\\s ")
        (delete-char 1))))

(defun my-cleanup-headline (int)
  "Does my clean-up on the close tag for a html headline tag. The
single parameter is the numeric part of the <hn>, i.e. 1 for <h1>,
etc."
  (let ((start nil) (end nil))
    (if (looking-at (format "</h%d>.+" int))
        (progn
          (setq start (point)
                end (+ (point) 5))
          (kill-region start end)
          (end-of-line)
          (yank)))))

;; My "advice" for the html-* commands begins here.

(defadvice html-image (after add-text activate compile)
  "I always put the alt attribute in my img tags."
  (save-excursion
    (forward-char 1)
    (insert " alt=\"\"")))

(defadvice html-paragraph (around insert-ptag activate compile)
  "I completely replace the html-paragraph command with my own because
I don't like what the sgml-mode html-paragraph command does. I'm lazy,
and I want it to wrap around any existing text block, and I like
well-formed documents, so it also inserts the </p> at the end of the
current text block."
  (if (not (looking-at blank-line))
      (save-excursion
        (backward-paragraph)
        (insert "\n<p>")
        (forward-paragraph)
        (insert "</p>\n"))
    (progn
      (insert "<p></p>\n")
      (re-search-backward "</p>" nil t))))
      
(defadvice html-headline-1 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-2 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-3 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-4 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-5 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-6 (before start-line activate compile)
  "I always put these at the beginning of a line."
  (my-start-line))

(defadvice html-headline-1 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 1))

(defadvice html-headline-2 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 2))

(defadvice html-headline-3 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 3))

(defadvice html-headline-4 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 4))

  (defadvice html-headline-5 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 5))

(defadvice html-headline-6 (after end-line activate compile)
  "I want to close it on the current line."
  (my-cleanup-headline 6))

(defadvice html-list-item (before start-line activate compile)
  "I always put these at the beginning of the line."
  (my-start-line))

(defadvice html-list-item (after close-litag activate compile)
  "I always close my li tags, too."
  (save-excursion
    (if (not (looking-at "\\s *$"))
        (end-of-line))
    (insert "</li>")))

(defadvice html-unordered-list (around close-litag activate compile)
  "I completely replaced html-unordered-list with my own code."
  (my-html-list "ul"))

(defadvice html-ordered-list (around close-litag activate compile)
  "I completely replaced html-ordered-list with my own code."
  (my-html-list "ol"))

;; I'm using it by putting (require 'html-advice) in my html-mode-hook
;; function. It could also be called by using an eval-after-load.

(provide 'html-advice)
