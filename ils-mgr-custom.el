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

;; Customizations for the Dell XPS13 from CW MARS, Inc..

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(current-language-environment "UTF-8")
 '(major-mode 'text-mode)
 '(indent-tabs-mode nil)
 '(tab-width 4)
 '(auto-save-default t)
 '(make-backup-files nil)
 '(require-final-newline t)
 '(diff-switches "-u")
 '(user-mail-address "jstephenson@cwmars.org")
 '(ispell-program-name "/usr/bin/enchant-2")
 '(safe-local-variable-values '((nxml-child-indent . 4) (indent-tab-mode)))
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
     ("util"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "dumbo")
      (sql-port 5432)
      (sql-database "util"))
     ("pg11"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5433))
     ("pg12"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5434))
     ("pg13"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5435))
     ("pg14"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5436))
     ("pg15"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-database "postgres")
      (sql-server "dumbo")
      (sql-port 5437))
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
      (sql-port 35434))))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1" "-v" "PROMPT2" "-v" "PROMPT3"))
 '(sql-product 'postgres)
 '(cwmars-db-updates-local-path "~/src/db-updates/")
 '(cwmars-apps-local-path "~/src/apps/")
 '(cwmars-apps-dev-local-path "~/src/apps_dev/")
 '(cwmars-utilities-local-path "~/src/utilities")
 '(cwmars-files-path "~/Documents/")
 '(cwmars-sql-path "~/src/sql/")
 '(cwmars-one-offs-path "~/src/one-offs/")
 '(cwmars-evergreen-path "~/src/Evergreen/")
 '(cwmars-ncipserver-path "~/src/NCIPServer/")
 '(cwmars-opensrf-path "~/src/OpenSRF/")
 '(cwmars-sipserver-path "~/src/SIPServer/"))
