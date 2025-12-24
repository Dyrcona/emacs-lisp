;; -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------
;; Copyright © 2023-2025 Jason J.A. Stephenson <jason@sigio.com>
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
(defgroup my-wesnoth nil "My Wesnoth utilities"
  :group 'local :prefix "my-wesnoth-")
(defcustom my-wesnoth-stable-data-dir "~/.var/app/org.wesnoth.Wesnoth/data/wesnoth/1.18/"
  "Path to my personal Wesnoth 1.18 data directory installed from Flatpak."
  :type 'directory :group 'my-wesnoth :require 'my-wesnoth)
(defcustom my-wesnoth-dev-data-dir "~/.local/share/wesnoth/1.19/"
  "Path to my personal Wesnoth 1.19 data directory installed from source."
  :type 'directory :group 'my-wesnoth :require 'my-wesnoth)
(defcustom my-wesnoth-flatpak-share-dir
  "/var/lib/flatpak/app/org.wesnoth.Wesnoth/current/active/files/share/wesnoth/"
  "Path to Wesnoth shared data files installed from Flatpak."
  :type 'directory :group 'my-wesnoth :require 'my-wesnoth)
(defcustom my-wesnoth-source-share-dir "/usr/local/wesnoth/share/wesnoth/"
  "Path to Wesnoth shared data files installed from source."
  :type 'directory :group 'my-wesnoth :require 'my-wesnoth)
(defcustom my-wesnoth-add-ons-dir "~/Src/Wesnoth/add-ons"
  "Path to source of my Wesnoth add-ons."
  :type 'directory :group 'my-wesnoth :require 'my-wesnoth)

;; Add wesnoth-mode for editing WML files.  Some keybindings below
;; here may depend on this one day.
(require 'wesnoth-mode)
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . wesnoth-mode))
;; Set up wesnoth-update for Wesnoth installed from source code.
(setq wesnoth-root-directory my-wesnoth-flatpak-share-dir
      wesnoth-update-output-directory "~/.emacs.d/elisp/"
      wesnoth-addition-file "~/.emacs.d/elisp/wesnoth-wml-additions.cfg")

;; Setup yet another keymap for Wesnoth.
(defvar my-wesnoth-map nil "Prefix map for my Wesnoth commands")
(define-prefix-command 'my-wesnoth-map)
(global-set-key (kbd "s-w") 'my-wesnoth-map)
(define-key my-wesnoth-map (kbd "a")
  (make-find-file-command (expand-file-name my-wesnoth-add-ons-dir)))
(define-key my-wesnoth-map (kbd "d")
  (make-find-file-command (expand-file-name my-wesnoth-stable-data-dir)))
(define-key my-wesnoth-map (kbd "s")
  (make-find-file-command (expand-file-name "saves" my-wesnoth-stable-data-dir)))
(define-key my-wesnoth-map (kbd "D")
  (make-find-file-command (expand-file-name my-wesnoth-dev-data-dir)))
(define-key my-wesnoth-map (kbd "S")
  (make-find-file-command (expand-file-name "saves" my-wesnoth-dev-data-dir)))
(define-key my-wesnoth-map (kbd "f")
  (make-find-file-command my-wesnoth-flatpak-share-dir))
(define-key my-wesnoth-map (kbd "F")
  (make-find-file-command my-wesnoth-source-share-dir))

(provide 'my-wesnoth)
