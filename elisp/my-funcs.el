;; ---------------------------------------------------------------
;; Copyright © 2022, 2023 Jason J.A. Stephenson <jason@sigio.com>
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

;; A collection of useful functions.

(defcustom my-vms-list
  '(("# My VMs" . "/home/jason/Src/other/hosts-vms.sedscr")
    ("# C/W MARS VMs" . "/home/jason/Src/other/hosts-cwmars.sedscr"))
  "Assoc list of hosts files comments and output file for sed commands."
  :type '(alist :key-type string :value-type file)
  :group 'local)

(defcustom my-hosts-file "/sudo:root@localhost:/etc/hosts"
  "Path used to open the hosts file."
  :type 'string :group 'local)

(defcustom my-bash-aliases-backup-dir "/home/jason/Src/other"
  "Directory where to save my .bash_aliases backup files."
  :type 'directory :group 'local)

(defun make-hosts-sedscripts ()
  "Function to copy blocks from my /etc/hosts file and write sed
commands to files to append the blocks to a file."
  (save-excursion
    (dolist (elem my-vms-list)
      (let ((regexp (concat "^" (car elem) "\n\\([^\n]+\n\\)+")))
        (goto-char (point-min))
        (if (re-search-forward regexp (point-max) t)
            (let ((text (match-string 0)))
              (with-temp-file (cdr elem)
                (insert "$a\n\n")
                (insert text)
                (goto-char (point-max))
                (forward-line -1)
                (while (re-search-backward "$" (point-min) t)
                  (replace-match "\\\\")
                  (beginning-of-line)))))))))

(defun backup-bash-aliases (arg)
  "Function to get name of .bash_aliases backup file."
  ;; We ignore the arg since this function is specific to 1 file.
  (cl-labels
      ((get-backup-version
        ()
        (let ((regexp "bash_aliases\\.\\([[:digit:]]+\\)\\'") (version 0))
          (dolist (inpath (directory-files my-bash-aliases-backup-dir nil regexp))
            (when (string-match regexp inpath)
              (let ((v (string-to-number (match-string 1 inpath))))
                (when (> v version)
                  (setq version v)))))
          (1+ version))))
    (expand-file-name
     (format "bash_aliases.%d" (get-backup-version))
     my-bash-aliases-backup-dir)))

(defmacro make-find-file-command (arg)
  "A shortcut to bind keys to find-file with an argument."
  `(lambda ()
     (interactive)
     (find-file ,arg)))

;; Numerical functions

(defun randint (start end)
  "Return pseudo-random integer between start and end."
  (if (> start end)
      (setq start (+ start end)
            end (- start end)
            start (- start end)))
  (+ (random (1+ (- end start))) start))

(defun average (&rest args)
  "Returns the average of its numerical arguments."
    (/ (apply '+ args) (length args)))

(defun raverage (&rest args)
  "Return the rounded average of its integer arguments. If the
modulus is greater than one half of the divisor, then the next
higher integer is returned, regardless of the evenness of the
result."
  (let* ((div (length args))
         (avg (/ (apply '+ args) div))
         (modulus (mod (apply '+ args) div))
         (comparison (if (cl-evenp div) '>= '>)))
    (if (funcall comparison modulus (/ div 2))
        (1+ avg)
      avg)))

;; Conversion Functions
(defun f->c (f)
  "Convert Fahrenheit to Centrigrade."
  (/ (* (- f 32) 5) 9))

(defun c->f (c)
  "Convert Centigrade to Fahrenheit."
  (+ (* (/ c 5) 9) 32))

(defun sqm->sqf (m)
  "Convert square meters to square feet."
  (* m 10.76391))

(defun sqf->sqm (f)
  "Convert square feet to square meters."
  (* f 0.092903))

(defun hectares->acres (h)
  "Convert hectares to acres."
  (* h 2.471052))

(defun acres->hectares (a)
  "Convert acres to hectares."
  (/ a 2.471052))

(provide 'my-funcs)
