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

;; Customizations for the Dell XPS13 from CW MARS, Inc..

(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (run-with-timer 0.05 nil 'my-resize-frame frame)))

;; Set up for CW MARS
(require 'cwmars)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default t)
 '(current-language-environment "UTF-8")
 '(cwmars-apps-dev-local-path "~/src/apps_dev/" nil (cwmars))
 '(cwmars-apps-local-path "~/src/apps/" nil (cwmars))
 '(cwmars-db-updates-local-path "~/src/db-updates/" nil (cwmars))
 '(cwmars-evergreen-path "~/src/Evergreen/" nil (cwmars))
 '(cwmars-files-path "~/Documents/" nil (cwmars))
 '(cwmars-ncipserver-path "~/src/NCIPServer/" nil (cwmars))
 '(cwmars-one-offs-path "~/src/one-offs/" nil (cwmars))
 '(cwmars-opensrf-path "~/src/OpenSRF/" nil (cwmars))
 '(cwmars-sipserver-path "~/src/SIPServer/" nil (cwmars))
 '(cwmars-sql-path "~/src/sql/" nil (cwmars))
 '(cwmars-tickets-path "/home/jstephenson/Documents/Tickets/" nil (cwmars))
 '(cwmars-utilities-local-path "~/src/utilities" nil (cwmars))
 '(diff-switches "-u")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/enchant-2")
 '(kill-whole-line t)
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(megamillions-csv-file-path "/run/media/jstephenson/Stuff/Src/lisp/megamillions.csv" nil (lotto))
 '(megamillions-numbers-file-path
   "/run/media/jstephenson/Stuff/Src/lisp/megamillions-numbers.txt" nil (lotto))
 '(my-dotfile-backup-dir "/home/jstephenson/Documents")
 '(my-vms-list
   '(("# C/W MARS VMs"
      . "/home/jstephenson/src/patches/hosts-cwmars.sedscr")))
 '(package-selected-packages
   '(adoc-mode bbcode-mode cargo cargo-mode csv csv-mode dokuwiki-mode
               flymake flyspell-correct markdown-mode php-mode
               python-mode rust-mode sql-indent sqlup-mode
               typescript-mode xml-rpc yaml-mode))
 '(powerball-csv-file-path "/run/media/jstephenson/Stuff/Src/lisp/powerball.csv" nil (lotto))
 '(powerball-numbers-file-path
   "/run/media/jstephenson/Stuff/Src/lisp/powerball-numbers.txt" nil (lotto))
 '(require-final-newline t)
 '(safe-local-variable-values '((nxml-child-indent . 4) (indent-tab-mode)))
 '(set-mark-command-repeat-pop t)
 '(sql-connection-alist
   '(("dumbo" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "dumbo") (sql-database "postgres") (sql-port 5432))
     ("egtest" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "dumbo") (sql-database "evergreen") (sql-port 5432))
     ("jasontest" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "dumbo") (sql-database "jasontest") (sql-port 5432))
     ("db1" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "localhost") (sql-database "evergreen")
      (sql-port 35432))
     ("db2" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "localhost") (sql-database "evergreen")
      (sql-port 35433))
     ("dev" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "localhost") (sql-database "evergreen")
      (sql-port 35434))
     ("noble" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "noble") (sql-database "evergreen") (sql-port 5432))
     ("resolute" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "resolute") (sql-database "evergreen")
      (sql-port 5432))
     ("bookworm" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "bookworm") (sql-database "evergreen")
      (sql-port 5432))
     ("trixie" (sql-product 'postgres) (sql-user "evergreen")
      (sql-server "trixie") (sql-database "evergreen") (sql-port 5432))))
 '(sql-interactive-mode-hook '(sqlup-mode))
 '(sql-mode-hook '(sqlup-mode))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1=%/> " "-v" "PROMPT2" "-v" "PROMPT3"))
 '(sql-product 'postgres)
 '(sqlup-blacklist '("csv" "data" "id" "label" "name" "path" "state"))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(user-mail-address "jstephenson@cwmars.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 143 :width normal)))))
