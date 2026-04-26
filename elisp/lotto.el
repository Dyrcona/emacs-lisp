;; -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------
;; Copyright © 2022-2026 Jason J.A. Stephenson <jason@sigio.com>
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

;; Functions and commands for lotteries and lottery data.

(defconst powerball-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Powerball/Winning_Numbers/powerball.csv")
(defconst powerball-csv-file-path "/home/jason/Src/lisp/powerball.csv")

(defconst megamillions-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/Winning_Numbers/megamillions.csv")
(defconst megamillions-csv-file-path "/home/jason/Src/lisp/megamillions.csv")

(defun munge-lottery-csv (lotto-name)
  "Clean up the lottery CSV files downloaded from the Texas
Lottery Commission website."
  (let ((header-string (if (string= lotto-name "Powerball") "Date,WB1,WB2,WB3,WB4,WB5,PB,PP\n"
                         "Date,WB1,WB2,WB3,WB4,WB5,MB,MP\n"))
        (main-regexp (concat "^" lotto-name ",\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)\\([^\r]+?\\)\r?$")))
    (when (re-search-forward main-regexp (point-max) nil 1)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-min))
    (insert header-string)
    (while (re-search-forward main-regexp (point-max) t)
      (replace-match (format "%04d-%02d-%02d%s" (string-to-number (match-string 3))
                             (string-to-number (match-string 1)) (string-to-number (match-string 2))
                             (match-string 4))))))

(defun get-powerball-results ()
  "Download Powerball results CSV from Texas Lottery Commission
and modify it for my own use.  Write the results to
/home/jason/Src/lisp/powerball.csv."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously powerball-csv-url)
    (munge-lottery-csv "Powerball")
    (write-file powerball-csv-file-path)))

(defun get-megamillions-results ()
  "Download Mega Millions results CSV from Texas Lottery Commission
and modify it for my own use.  Write the results to
/home/jason/Src/lisp/megamillions.csv."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously megamillions-csv-url)
    (munge-lottery-csv "Mega Millions")
    (write-file megamillions-csv-file-path)))

(defun fix-lotto-numbers (start end)
  "Fix {powerball,megamillions}-numbers.txt entries that were
downloaded from the State of Texas lottery site using my
get-{powerball,megamillions}-results commands."
  (interactive "r")
  (unless (= (point) end)
    (goto-char end))
  (while (re-search-backward "^\\([0-9-]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)?" start t)
    (replace-match
     (format "%s  %02d  %02d  %02d  %02d  %02d  %02d  %d" (match-string 1)
             (string-to-number (match-string 2)) (string-to-number (match-string 3))
             (string-to-number (match-string 4)) (string-to-number (match-string 5))
             (string-to-number (match-string 6)) (string-to-number (match-string 7))
             (string-to-number (or (match-string 8) "0"))))))

;; Bind keys for lottery commands
(defvar-keymap my-lotto-command-map
  :doc "For binding lottery commands"
  "f" 'fix-lotto-numbers
  "m" 'get-megamillions-results
  "p" 'get-powerball-results)
(keymap-set mode-specific-map "l" my-lotto-command-map)

(provide 'lotto)
