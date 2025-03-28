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

(defun consume-line (&optional n)
  "Consume, i.e. delete and return as a string, `n` lines from
buffer. Returns nil if there are no more lines to consume."
  (if (not (bolp))
      (beginning-of-line))
  (if (or (and n (< n 0) (bobp)) (eobp))
      nil
    (let ((start (point)))
      (forward-line n)
      (delete-and-extract-region start (point)))))

(provide 'consume-line)
