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

;; Customizations for needle.

;; Use bash on mail.sigio.com:
(require 'tramp)
(add-to-list 'tramp-connection-properties
             (list "/ssh:\\(jason@\\|store@\\|root@\\)?mail"
                   "remote-shell" "/usr/local/bin/bash"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default t)
 '(current-language-environment "UTF-8")
 '(diff-switches "-u")
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "/bin/sbcl --noinform")
 '(inhibit-startup-screen t)
 '(ispell-program-name "/bin/enchant-2")
 '(kill-whole-line t)
 '(major-mode 'text-mode)
 '(make-backup-files nil)
 '(my-dotfile-backup-dir "/home/jason/Src/other/needle")
 '(package-selected-packages
   '(rust-playground cargo rust-mode bbcode-mode dokuwiki-mode ac-slime auto-complete lua-mode yaml-mode sqlup-mode markdown-mode csv-mode csv))
 '(python-shell-extra-pythonpaths '("/home/jason/Src/python"))
 '(require-final-newline t)
 '(safe-local-variable-values
   '((nxml-child-indent . 4)
     (indent-tab-mode)
     (make-backup-files . t)
     (make-backup-file-name-function . my-dotfile-backup-name)))
 '(set-mark-command-repeat-pop t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(user-mail-address "jason@sigio.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 120 :width normal))))
 '(region ((t (:background "light gray" :distant-foreground "gtk_selection_fg_color")))))
