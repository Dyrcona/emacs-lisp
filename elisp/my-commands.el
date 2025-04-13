;; -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------
;; Copyright © 2022-2025 Jason J.A. Stephenson <jason@sigio.com>
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

;; A collection of interactive commands for editing and other
;; purposes.

;; cl-lib is usually loaded for me, but just making sure.
(require 'cl-lib)

;; For my get-url command.
(require 'url)

;; Delete all whitespace up to next word. Supposed to work like
;; c-hungry-delete-forward.
(defun hungry-delete-forward ()
  "Delete all whitespace up to next word."
  (interactive)
  (let ((regexp "[[:space:]\r\n]+"))
    (if (looking-at regexp)
        (progn
          (re-search-forward regexp nil t)
          (replace-match "" nil nil)))))

(defun count-names (beginning end)
  "Count the last names in selected region of names.txt buffer
and print results sorted in another buffer that replaces the
current buffer.  Count first names if called with universal
argument."
  (interactive "r")
  (let ((a-list ()) (regexp "\\([[:word:]-]+\\)"))
    (if current-prefix-arg
        (setq regexp (concat "^" regexp))
      (setq regexp (concat regexp "$")))
    (goto-char beginning)
    (while (re-search-forward regexp end t)
      (let ((name (match-string 1)))
        (if (assoc name a-list)
            (rplacd (assoc name a-list) (1+ (cdr (assoc name a-list))))
          (setq a-list (cl-acons name 1 a-list)))))
    (let ((results-buffer (generate-new-buffer "results")))
      (with-current-buffer results-buffer
        (dolist (elem a-list)
          (insert (format "%s %d\n" (car elem) (cdr elem))))
        (sort-numeric-fields 2 (point-min) (point-max))
        (reverse-region (point-min) (point-max))
        (goto-char (point-min))
        (display-buffer-same-window results-buffer nil)))))

(defun fix-initials ()
  "Remove all initials in names.txt and replace it with
random name beginning with the same letter as the initial."
  (interactive)
  (save-excursion
    (setq case-fold-search nil)
    (cl-labels
        ((find-rnd-name
          (regexp last)
          (let ((lst ()))
            (goto-char (point-min))
            (while (re-search-forward regexp (point-max) t)
              (goto-char (match-end 0))
              (if (or last (not (looking-at "$")))
                  (setq lst (cons (match-string 1) lst))))
            (nth (random (length lst)) lst)))
         (search-and-replace
          (regexp last)
          (goto-char (point-min))
          (while (re-search-forward regexp (point-max) t)
            (let* ((return-to (match-beginning 1))
                   (nregexp (format "\\_<\\(%s[[:word:]-]+\\)%s" (match-string 1) (if last "$" "")))
                   (name (find-rnd-name nregexp last)))
              (goto-char return-to)
              (delete-char 2)
              (insert name)))))
      ;; Do last initials.
      (search-and-replace "\\([A-Z]\\)\\.$" t)
      ;; Do first initials.
      (search-and-replace "^\\([A-Z]\\)\\. " nil))))

(defun ilength (start end)
  "Interactive version of the length function."
  (interactive "r")
  (message (format "%d" (length (buffer-substring-no-properties start end)))))

(defvar default-upstream-branch "origin/master"
  "Default upstream branch for git-cherrylog.")
(defvar default-local-branch nil
  "Default local branch for git-cherrylog.")

(defun git-cherrylog (upstream head)
  "Run git cherry on UPSTREAM and HEAD to get commit log entries
for the commits in HEAD that are not in UPSTREAM."
  (interactive
   (cl-labels
       ((get-branch-list (&optional remote)
         (let ((command (format "git branch %s" (if remote "-r" "")))
               (branches '()))
           (with-temp-buffer
             (shell-command command t)
             (if remote
                 (flush-lines "^* "))
             (goto-char (point-min))
             (while (re-search-forward "^*? +\\([^ ]+\\)$" (point-max) t)
               (let ((line (match-string 0))
                     (branch (match-string 1)))
                 (setq branches (append branches (list branch)))
                 (if (and (not remote) (string-match "^*" line))
                     (setq default-local-branch branch)))))
           branches)))
     (let* ((upstream-hist (get-branch-list t))
            (local-hist (get-branch-list))
            (upstream-pos (cl-position default-upstream-branch upstream-hist :test 'string=))
            (local-pos (cl-position default-local-branch local-hist :test 'string=)))
       (list
        (read-string "Branch or commit for upstream: " nil `(upstream-hist . ,upstream-pos)
                     default-upstream-branch)
        (read-string "Branch or commit for head: " nil `(local-hist . ,local-pos) default-local-branch)))))
  (let ((cherry-buffer (get-buffer-create "*cherrylog*")))
    (with-current-buffer cherry-buffer
      (erase-buffer)
      (shell-command (format "git cherry %s %s" upstream head) t)
      (keep-lines "^+ ")
      (goto-char (point-min))
      (while (re-search-forward "^+ \\([a-f0-9]+\\)$" (point-max) t)
        (let ((commit-hash (match-string 1)))
          (replace-match "")
          (shell-command (format "git log -n 1 %s" commit-hash) t)))
      (goto-char (point-min)))
    (switch-to-buffer cherry-buffer)))

(defun get-url (url &optional start end)
  "Get a URL and switch to its buffer."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (list (read-string "URL: ") nil nil)))
  (if (not url)
      (setq url (buffer-substring-no-properties start end)))
  (url-retrieve url (lambda (_status) (switch-to-buffer (current-buffer)))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun unfill-region (b e)
  "Do the opposite of fill-region."
  (interactive "*r")
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-region b e)))

(defun goto-random-line ()
  "Go to a random line in the current buffer."
  (interactive)
  (let ((lines (count-lines (point-min) (point-max))))
    (goto-char (point-min))
    (forward-line (random lines))))

;; close-and-kill-this-pane taken from EmacsWiki: https://www.emacswiki.org/emacs/KillingBuffers#h5o-5
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill
 the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; close-and-kill-next-pane taken from EmacsWiki: https://www.emacswiki.org/emacs/KillingBuffers#h5o-5
(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and
 kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(provide 'my-commands)
