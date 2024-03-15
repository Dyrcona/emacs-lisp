;; ---------------------------------------------------------------
;; Copyright © 2022-2024 Jason J.A. Stephenson <jason@sigio.com>
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
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(my-vms-list
   '(("# C/W MARS VMs" . "/home/jstephenson/src/patches/hosts-cwmars.sedscr")))
 '(package-selected-packages
   '(cargo rust-mode ac-slime slime auto-complete bbcode-mode dokuwiki-mode markdown-mode csv yaml-mode))
 '(require-final-newline t)
 '(safe-local-variable-values '((nxml-child-indent . 4) (indent-tab-mode)))
 '(set-mark-command-repeat-pop t)
 '(sql-connection-alist
   '(("dumbo"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5432))
     ("jasontest"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "dumbo")
      (sql-port 5432)
      (sql-database "jasontest"))
     ("pg10"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5433))
     ("db1"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "evergreen")
      (sql-server "localhost")
      (sql-port 35432))
     ("db2"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "evergreen")
      (sql-server "localhost")
      (sql-port 35433))
     ("training"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "evergreen")
      (sql-server "localhost")
      (sql-port 35434))
     ("focal"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "focal")
      (sql-database "evergreen")
      (sql-port 5432))
     ("jammy"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "jammy")
      (sql-database "evergreen")
      (sql-port 5432))
     ("bullseye"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "bullseye")
      (sql-database "evergreen")
      (sql-port 5432))
     ("bookworm"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "bookworm")
      (sql-database "evergreen")
      (sql-port 5432))))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1" "-v" "PROMPT2" "-v" "PROMPT3"))
 '(sql-product 'postgres)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-connection-properties
   '(("/ssh:\\(jason\\|root\\|store\\)@mail\\.sigio\\.com" "remote-shell" "/usr/local/bin/bash")))
 '(user-mail-address "jstephenson@cwmars.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "light gray" :distant-foreground "gtk_selection_fg_color")))))
