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

;; Customization for my personal Dell XPS 13.

;; Setup for SLIME
(require 'auto-complete)
(require 'slime)
(require 'slime-autoloads)
(load (expand-file-name "~/quicklisp/slime-helper"))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform"
      slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root "file:///home/jason/Documents/Programming%20Documentation/Lisp/HyperSpec/")
(slime-setup '(slime-fancy))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; Use bash on mail.sigio.com:
(require 'tramp)
(add-to-list 'tramp-connection-properties
             (list "/ssh:\\(jason@\\|store@\\|root@\\)?mail"
                   "remote-shell" "/usr/local/bin/bash"))
;; Ditto for the host beastie:
(add-to-list 'tramp-connection-properties
             (list "/ssh:\\(jason@\\|root@\\)?beastie"
                   "remote-shell" "/usr/local/bin/bash"))

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
 '(user-mail-address "jason@sigio.com")
 '(ispell-program-name "/usr/bin/enchant-2")
 '(package-selected-packages '(ac-slime lua-mode sqlup-mode csv-mode geiser))
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
     ("buster"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "buster")
      (sql-database "evergreen")
      (sql-port 5432))
     ("bullseye"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "bullseye")
      (sql-database "evergreen")
      (sql-port 5432))
     ("beowulf"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "beowulf")
      (sql-database "evergreen")
      (sql-port 5432))
     ("chimaera"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "chimaera")
      (sql-database "evergreen")
      (sql-port 5432))))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1" "-v" "PROMPT2" "-v" "PROMPT3"))
 '(sql-product 'postgres))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "light gray" :distant-foreground "gtk_selection_fg_color")))))
