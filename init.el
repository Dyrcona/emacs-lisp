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

;; Set up package.el
(require 'package)
(add-to-list 'package-archives
             (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Setup load path for finding my files:
(add-to-list 'load-path "~/.emacs.d/elisp")
;; Byte-recompile the libraries, if needed:
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Load my libraries that I want all the time.
(require 'buffer-advice)
(require 'my-funcs)
(require 'my-commands)
(require 'my-skeletons)
(require 'funcall-region)
(require 'unscroll)

;; Start emacs server process so we can use emacsclient as the editor
;; for other programs.
(require 'server)
(unless (file-exists-p (expand-file-name server-name server-socket-dir))
    (server-start))

;; Turn on upcase-region and downcase-region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Turn on narrow-to-region.
(put 'narrow-to-region 'disabled nil)
;; Enable erase-buffer for interactive use.
(put 'erase-buffer 'disabled nil)

;; Unset global keys
;; Turn off Ctrl-z.  I almost never mean to use it.
(if (display-graphic-p)
    (global-unset-key "\C-z"))

;; Set Global keys
;; The unscroll command from unscroll.el.
(global-set-key [?\C-,] 'unscroll)
;; hungry-delete-forward in all modes.
(global-set-key [?\C-*] 'hungry-delete-forward)
;; To get length of the selection.
(global-set-key [?\C-$] 'ilength)
;; Been using revert-buffer alot.
(global-set-key [?\C-!] 'revert-buffer)
;; Map describe-char to Ctrl-# because it's quicker.
(global-set-key [?\C-#] 'describe-char)
;; C-% is bound in my-skeletons.el.
;; (global-set-key [?\C-%] 'skeleton-next-position)
;; Insert a random letter followed by period.
(global-set-key [?\C-^]
                (lambda ()
                  (interactive)
                  (insert (format "%c." (+ (random 26) 65)))))
;; Run join-line.
(global-set-key [?\C-&] 'join-line)

;; Bind unfill-paragraph from Stefan Monnier.
(global-set-key "\M-Q" 'unfill-paragraph)

;; Bind goto-random-line to M-g-r as a complement to M-g-g
;; (goto-line).
(global-set-key "\M-gr" 'goto-random-line)

;; Map view-file family of commands.
(define-key ctl-x-4-map "v" 'view-file-other-window)
(define-key ctl-x-5-map "v" 'view-file-other-frame)

;; Add unfill-region to the Edit menu.
(define-key-after (lookup-key global-map [menu-bar edit]) [unfill]
  '(menu-item "Unfill" unfill-region . (:enable (use-region-p))) 'fill)

;; Super-j prefix for more custom bindings.
(defvar super-j-map)
(define-prefix-command 'super-j-map)
(global-set-key (kbd "s-j") 'super-j-map)
(define-key super-j-map (kbd "0") 'close-and-kill-this-pane)
(define-key super-j-map (kbd "1") 'close-and-kill-next-pane)
(define-key super-j-map (kbd "!") 'erase-buffer)
(define-key super-j-map (kbd "$") 'urldecode-region)
(define-key super-j-map (kbd "%") 'urlencode-region)
(define-key super-j-map (kbd "b") 'ispell-buffer)
(define-key super-j-map (kbd "cd") 'copy-directory)
(define-key super-j-map (kbd "cf") 'copy-file)
(define-key super-j-map (kbd "dc") 'desktop-clear)
(define-key super-j-map (kbd "f") 'fill-region)
(define-key super-j-map (kbd "g") 'get-url)
(define-key super-j-map (kbd "i") 'insert-buffer)
(define-key super-j-map (kbd "m") 'emerge-files)
(define-key super-j-map (kbd "n") 'insert-names)
(define-key super-j-map (kbd "r") 'ispell-region)
(define-key super-j-map (kbd "s") 'signed-off-by)
(define-key super-j-map (kbd "u") 'unfill-region)
(define-key super-j-map (kbd "w") 'count-words)

;; Add git to VC backends.
(add-to-list 'vc-handled-backends 'Git)

;; I don't use IDL, but I do edit qmake project files.
(setq auto-mode-alist (rassq-delete-all 'idlwave-mode auto-mode-alist)
      auto-mode-alist (cons '("\\.pro\\'" . conf-mode) auto-mode-alist))

;; Setup po-mode for translating software.
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; Set diff-mode for reverse patch (.rpatch) files.
(add-to-list 'auto-mode-alist '("\\.rpatch\\'" . diff-mode))

;; Use cperl-mode by default
(defalias 'perl-mode 'cperl-mode)
(require 'cperl-mode)
;; cperl-mode doesn't have an add-style command, so we create our
;; style in a defun.
(defun evergreen-perl-style ()
  "Set cperl-mode for Evergreen coding guidelines."
  (setq cperl-indent-level  4
        cperl-brace-offset 0
        cperl-continued-brace-offset 0
        cperl-label-offset -4
        cperl-continued-statement-offset 4
        cperl-close-paren-offset -4
        cperl-indent-parens-as-block t
        cperl-merge-trailing-else t
        cperl-indent-left-aligned-comments t))

;; cperl-mode hook
(add-hook 'cperl-mode-hook
          (lambda ()
            (evergreen-perl-style)
            ;; Because I might have a different Perl style for
            ;; different files one day, I set the below here.
            (setq cperl-tab-always-indent t
                  cperl-indent-parens-as-block t
                  cperl-electric-parens t
                  cperl-electric-parens-mark t
                  cperl-electric-keywords t)
            (local-set-key (kbd "C-c C-d") 'hungry-delete-forward)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)))

;; lisp modes
;; A generic hook
(defun my-lisp-modes-hook ()
  (local-set-key (kbd "C-<tab>") 'indent-sexp))

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'my-lisp-modes-hook)
;; lisp-mode
(add-hook 'lisp-mode-hook 'my-lisp-modes-hook)
;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook 'my-lisp-modes-hook)

;; scheme-mode
(add-hook 'scheme-mode-hook 'my-lisp-modes-hook) ;Reuse lisp-mode-hook, for now.
;; Use guile with run-scheme.
(setq scheme-program-name "guile")
;; Use guile with geiser.
(setq geiser-active-implementations '(guile))

;; Setup for erlang mode.
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode t)))

;; For editing Mac OS X strings and plist files, I believe I want to use
;; utf-16-le encoding (an alias of utf-16le-with-signature).
(modify-coding-system-alist 'file "\\.strings$" 'utf-16-le)

;; Makefile mode:
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 4)))

;; Do cc-mode initialization.
;; Use objc-mode on Objective-C++ files:
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
;; WebKit.org indentation style:
(defconst webkit-c-style
  '((c-basic-offset                 . 4)
    (c-echo-syntactic-information-p . t)
    (c-offsets-alist                . ((innamespace      . 0)
                                       (inline-open      . 0)
                                       (label            . 0)
                                       (statement-cont   . +)))
    )
  "WebKit Style")

;; My indentation style:
(defconst my-c-style
  '((c-basic-offset . 2)
    (tab-width . 2)
    (c-offsets-alist . ((innamespace . 0)
                        (inline-open . 0)
                        (inextern-lang . 0)
                        (label . 0)
                        (statement-cont . 0)
                        (substatement-open . 0)
                        (arglist-close . 0))))
  "My Style")

;; Since this is slightly more complicated than usual, I actually
;; define a fun rather than use a lambda expression.
(defun my-c-mode-common-hook ()
  (c-add-style "WebKit" webkit-c-style)
  (c-add-style "mine" my-c-style)
  (cond
   ((and (buffer-file-name) (string-match "Src/WebKit" (buffer-file-name)))
    (c-set-style "WebKit"))
   (t (c-set-style "mine"))))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Do initialization for SQL Mode.
(add-to-list 'auto-mode-alist '("\\.pg\\'" . sql-mode))

;; Do initialization for HTML Mode.
(add-hook 'html-mode-hook
          (lambda ()
            (require 'html-advice)))

;; Setup for editing python.
;; Make sure the module is loaded.
(require 'python)
;; Setup style according to PEP 8.  python-mode does most of it for
;; us.  I just make sure Emacs doesn't sneak any tabs in.
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width python-indent-offset)))

;; Add a hook for conf-mode[Space] to make sed scripts whenever I edit
;; and save /etc/hosts.
(add-hook 'conf-space-mode-hook
          (lambda ()
            (if (and (buffer-file-name) (string-match my-hosts-file (buffer-file-name)))
                (add-hook 'after-save-hook 'make-hosts-sedscripts nil t))))

;; Add a hook to make scripts executable when they are saved.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Copy .bash_profile, .bashrc, or .bash_aliases after save
(unless (string-equal (system-name) "ILS-MGR")
  (add-hook 'sh-mode-hook
            (lambda ()
              (if (and (buffer-file-name)
                       (string-match-p "\\.bash\\(_profile\\|_aliases\\|rc\\)"
                                       (buffer-file-name)))
                  (add-hook 'after-save-hook 'my-copy-dotfile nil t)))))

;; Setup for SLIME
(require 'auto-complete)
(require 'slime)
(require 'slime-autoloads)
(load (expand-file-name "~/quicklisp/slime-helper"))
(setq slime-net-coding-system 'utf-8-unix
      common-lisp-hyperspec-root
      (concat "file://" (getenv "HOME") "/Documents/Programming%20Documentation/Lisp/HyperSpec/"))
(slime-setup '(slime-fancy))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(global-set-key [(f7)] 'slime-hyperspec-lookup)

;; Load machine-specific customizations
(load
 (customize-set-variable
  'custom-file
  (expand-file-name
   (downcase (format "%s-custom.el" (system-name)))
   user-emacs-directory)))
