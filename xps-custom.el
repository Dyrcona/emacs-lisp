;; -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------
;; Copyright © 2022-2025 Jason J.A. Stephenson <jason@sigio.com>
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

;; Ediff frames are oddly sized on Emacs 29.4 on Arch with
;; Gnome/Wayland.
(defun xps-resize-frame (frame)
  "Make Ediff frames legible on Wayland."
  (let ((title (frame-parameter frame 'title)))
    (if (and title (string-equal-ignore-case title "ediff"))
        (set-frame-size frame 18 3)))) ; 18x3 picked after experimentation

(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (run-with-timer 0.05 nil 'xps-resize-frame frame)))

;; Add my local INFOPATH for texinfo documentation
(add-to-list 'Info-directory-list (expand-file-name "~/share/info"))

(require 'lotto)
;; SLIME Setup
(require 'auto-complete)
(require 'slime)
(require 'slime-autoloads)
(setq slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root
      (concat "file://" (getenv "HOME") "/Documents/Programming%20Documentation/Lisp/HyperSpec/"))
(slime-setup '(slime-fancy))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(global-set-key [(f7)] 'slime-hyperspec-lookup)

;; scheme-mode
(add-hook 'scheme-mode-hook 'my-lisp-modes-hook) ;Reuse lisp-mode-hook, for now.
;; Use guile with run-scheme.
(setq scheme-program-name "guile")
;; Use guile with geiser.
(setq geiser-active-implementations '(guile))

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

;; Mostly managed by custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "/usr/bin/sbcl --noinform" t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/enchant-2")
 '(kill-whole-line t)
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(package-selected-packages
   '(adoc-mode pg cargo rust-mode cmake-mode ac-slime bbcode-mode csv-mode dokuwiki-mode geiser geiser-guile markdown-mode sqlup-mode yaml-mode))
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
     ("dev"
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
      (sql-port 5432))))
 '(sql-postgres-options
   '("-P" "pager=off" "-v" "PROMPT1=%/> " "-v" "PROMPT2" "-v" "PROMPT3"))
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
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 143 :width normal)))))
