;; Copyright © 1997 O'Reilly Media & Associates, Inc.
;;
;; The following code is taken from WRITING GNU EMACS EXTENSIONS by
;; Bob Glickstein, published 1997 by O'Reilly Media & Associates,
;; Inc., Sebastopol, CA.

(defvar unscroll-point (make-marker)
  "Cursor position for next `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next `unscroll'.")
(defvar unscroll-hscroll nil "Hscroll for next `unscroll'.")

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defun unscroll-maybe-remember (&rest args)
  (if (not (and (symbolp last-command)
                (get last-command 'unscrollable)))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(advice-add 'scroll-up :before #'unscroll-maybe-remember)

(advice-add 'scroll-down :before #'unscroll-maybe-remember)

(advice-add 'scroll-right :before #'unscroll-maybe-remember)

(advice-add 'scroll-left :before #'unscroll-maybe-remember)

(provide 'unscroll)
