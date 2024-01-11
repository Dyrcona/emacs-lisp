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

;; Add my local INFOPATH for texinfo documentation
(add-to-list 'Info-directory-list (expand-file-name "~/share/info"))

;; Set up for Lua
(setq auto-mode-alist (cons '("\\.lua\\'" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; Set up for PHP
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("\\.php[345]?\\'" . php-mode)) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

;; Set up for CW MARS
(require 'cwmars)

;; Set up for wesnoth-mode
(require 'my-wesnoth)

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

;; Mostly managed by custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/enchant-2")
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(package-selected-packages
   '(cmake-mode ac-slime bbcode-mode csv-mode dokuwiki-mode geiser geiser-guile markdown-mode sqlup-mode yaml-mode))
 '(require-final-newline t)
 '(safe-local-variable-values
   '((make-backup-file-name-function . my-dotfile-backup-name)
     (make-backup-files . t)
     (nxml-child-indent . 4)
     (indent-tab-mode . t)))
 '(set-mark-command-repeat-pop t)
 '(sql-connection-alist
   '(("db1"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "localhost")
      (sql-database "evergreen")
      (sql-port 35432))
     ("db2"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "localhost")
      (sql-database "evergreen")
      (sql-port 35433))
     ("training"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "localhost")
      (sql-database "evergreen")
      (sql-port 35434))
     ("jasontest"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "dumbo")
      (sql-database "jasontest")
      (sql-port 5432))
     ("pg10"
      (sql-product 'postgres)
      (sql-user "evergreen")
      (sql-server "dumbo")
      (sql-database "postgres")
      (sql-port 5433))))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1" "-v" "PROMPT2" "-v" "PROMPT3"))
 '(sql-product 'postgres)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
 '(tramp-connection-properties
   '(("/ssh:\\(jason@\\|root@\\)?beastie" "remote-shell" "/usr/local/bin/bash")
     ("/ssh:\\(jason@\\|store@\\|root@\\)?mail\\(\\.sigio\\.com\\)?" "remote-shell" "/usr/local/bin/bash")
     ("/sshfs:" "direct-async-process" t)))
 '(user-mail-address "jason@sigio.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight medium :height 128 :width normal)))))
