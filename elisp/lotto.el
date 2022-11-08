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

;; Functions and commands for lotteries and lottery data.

(defconst powerball-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Powerball/Winning_Numbers/powerball.csv")
(defconst powerball-csv-file-path "/home/jason/Src/lisp/powerball.csv")

(defconst megamillions-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/Winning_Numbers/megamillions.csv")
(defconst megamillions-csv-file-path "/home/jason/Src/lisp/megamillions.csv")

;; The current Powerball format (choose 5 out of 69 and 1 from 26)
;; began on 10/07/2015.  The following value is the line for that
;; entry in the powerball.csv file.
(defconst powerball-current-csv-line 594)

;; The curren Mega Millions format (choose 5 from 1 to 70 and 1 from
;; 25) began on 10/31/2017.  The following value is the line for that
;; entry in the megamillions.csv file.
(defconst megamillions-current-csv-line 1453)

(defun munge-lottery-csv (lotto-name)
  "Clean up the lottery CSV files downloaded from the Texas
Lottery Commission website."
  (let ((header-string (if (string= lotto-name "Powerball") "Date,WB1,WB2,WB3,WB4,WB5,PB,PP\n"
                         "Date,WB1,WB2,WB3,WB4,WB5,MB,MP\n"))
        (main-regexp (concat "^" lotto-name ",\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)\\([^\r]+\\)\r")))
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

(defun fix-powerball-numbers (start end)
  "Fix powerball-numbers.txt entries that were downloaded from
the State of Texas lottery site using my get-powerball-results
command."
  (interactive "r")
  (unless (= (point) end)
    (goto-char end))
  (reverse-region start end)
  (while (re-search-backward "^\\([0-9-]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)" start t)
    (replace-match (format "%s  %02d  %02d  %02d  %02d  %02d  %02d  %d" (match-string 1)
                           (string-to-number (match-string 2)) (string-to-number (match-string 3))
                           (string-to-number (match-string 4)) (string-to-number (match-string 5))
                           (string-to-number (match-string 6)) (string-to-number (match-string 7))
                           (string-to-number (match-string 8))))))

(provide 'lotto)
