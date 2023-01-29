;; ---------------------------------------------------------------
;; Copyright © 2023 Jason J.A. Stephenson <jason@sigio.com>
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

;; Make sure that my functions have loaded because we use them here.
(require 'my-funcs)

;; Make some variables modifiable by custom in case paths are
;; different on my other computers.
(defcustom my-wesnoth-data-dir "~/.var/app/org.wesnoth.Wesnoth/data/wesnoth/1.16/"
  "Path to my personal Wesnoth 1.16 data directory installed from Flatpak."
  :type 'directory :group 'local :require 'my-wesnoth)

;; Add wesnoth-mode for editing WML files.  Some keybindings below
;; here may depend on this one day.
(require 'wesnoth-mode)
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . wesnoth-mode))

;; Setup yet another keymap for Wesnoth.
(defvar my-wesnoth-map nil "Prefix map for my Wesnoth commands")
(define-prefix-command 'my-wesnoth-map)
(global-set-key (kbd "s-w") 'my-wesnoth-map)
(define-key my-wesnoth-map (kbd "d")
  (make-find-file-command (expand-file-name my-wesnoth-data-dir)))

(provide 'my-wesnoth)
