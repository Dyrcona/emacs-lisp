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

(defgroup my-lotto nil "My lottery functions"
  :group 'local :prefix "my-lotto-")
(defcustom powerball-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Powerball/Winning_Numbers/powerball.csv"
  "URL to download the Powerball winning numbers CSV file."
  :type 'string :group 'my-lotto)
(defcustom powerball-csv-file-path "/home/jason/Src/lisp/powerball.csv"
  "Path to the downloaded Powerball winning numbers CSV file."
  :type 'file :group 'my-lotto)

(defcustom megamillions-csv-url
  "https://www.texaslottery.com/export/sites/lottery/Games/Mega_Millions/Winning_Numbers/megamillions.csv"
  "URL to download the MegaMillions winning numbers CSV file."
  :type 'string :group 'my-lotto)
(defcustom megamillions-csv-file-path "/home/jason/Src/lisp/megamillions.csv"
  "Path to the downloaded MegaMillions winning numbers CSV file."
  :type 'file :group 'my-lotto)

;; For after tax winnings estimation
(defcustom us-tax-rate 3.7e-1
  "Highest US marginal tax rate."
  :type 'float :group 'my-lotto)
(defcustom state-tax-rate 5.0e-2
  "Higest state marginal tax rate."
  :type 'float :group 'my-lotto)
(defcustom state-surtax-rate 4.0e-2
  "State surtax on high income, if any."
  :type 'float :group 'my-lotto)
(defcustom state-surtax-floor 1.10775e6
  "Amount where the state surtax kicks in, if any."
  :type 'float :group 'my-lotto)

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

(defun lottery-winnings (amount)
  "Stupid little function to esitmate after tax winnings
from a lottery jackpot in MA."
  (let* ((us-tax (* amount us-tax-rate))
         (state-base-tax (* amount state-tax-rate))
         (state-surtax
          (if (and (> state-surtax-rate 0.0) (> amount state-surtax-floor))
              (* (- amount state-surtax-floor) state-surtax-rate)
            0.0))
         (state-tax (+ state-base-tax state-surtax)))
    `((gross-amount . ,amount)
      (net-amount . ,(- amount us-tax state-tax))
      (us-taxes . ,us-tax)
      (state-taxes . ,state-tax)
      (total-taxes . ,(+ us-tax state-tax)))))

(defun summarize-winnings (winnings)
  "Pretty print the results of the lottery-winnings function."
  (interactive "*nLottery Winnings: ")
  (cl-flet
      ((format-symbol (symb)
         (string-pad
          (mapconcat
           (lambda (x)
             (if (= 2 (length x))
                 (upcase x)
               (capitalize x)))
           (split-string (symbol-name symb) "-")
           " ")
          12
          ?\s
          t)))
    (dolist (k (lottery-winnings winnings))
      (insert (format-symbol (car k)))
      (insert ": $")
      (insert (group-number (floor (cdr k))))
      (insert "\n"))))

;; Bind keys for lottery commands
(defvar-keymap my-lotto-command-map
  :doc "For binding lottery commands"
  "f" 'fix-lotto-numbers
  "m" 'get-megamillions-results
  "p" 'get-powerball-results
  "s" 'summarize-winnings)
(keymap-set mode-specific-map "l" my-lotto-command-map)

(provide 'lotto)
