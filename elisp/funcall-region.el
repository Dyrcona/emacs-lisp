;; -*- lexical-binding: t; -*-
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

;; A collection of code for running functions over regions.

;; This library is based on code stolen from
;; https://stackoverflow.com/a/6541072

(defun funcall-region (start end func)
  "Run a function that takes a string argument over the region
between START and END in current buffer, replacing the region
with the output of the function."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defmacro make-funcall-region-command (cmd func-symbol &optional docstring)
  "Make a command, named cmd, using funcall-region to call
func-symbol over region.  func-symbol is a function that takes a
string argument."
  `(defun ,(intern cmd) (start end)
     ,docstring
     (interactive "r")
     (funcall-region start end ,func-symbol)))

(make-funcall-region-command
 "urlencode-region"
 'url-hexify-string
 "URLencode the region between START and END in current buffer.")

(make-funcall-region-command
 "urldecode-region"
 'url-unhex-string
 "De-URLencode the region between START and END in current buffer.")

(provide 'funcall-region)
