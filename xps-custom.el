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

;; Customization for my personal Dell XPS 13.

(setenv "SSH_AUTH_SOCK" "/run/user/1000/keyring/ssh")

;; Add my local INFOPATH for my own texinfo documentation
(add-to-list 'Info-directory-list (expand-file-name "~/share/info"))

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

;; Load my configuration for working on Battle for Wesnoth maps and code.
(require 'my-wesnoth)

;; Mostly managed by custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default t)
 '(current-language-environment "UTF-8")
 '(diff-switches "-u")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/enchant-2")
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(package-selected-packages
   '(markdown-mode dokuwiki-mode bbcode-mode yaml-mode ac-slime sqlup-mode csv-mode geiser))
 '(python-shell-extra-pythonpaths '("/home/jason/Src/python"))
 '(require-final-newline t)
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
     ("pg15"
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
 '(sql-product 'postgres)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(user-mail-address "jason@sigio.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "light gray" :distant-foreground "gtk_selection_fg_color")))))
