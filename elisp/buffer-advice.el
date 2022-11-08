;; Copyright © 1997 O'Reilly Media & Associates, Inc.
;;
;; The following code is taken from WRITING GNU EMACS EXTENSIONS by
;; Bob Glickstein, published 1997 by O'Reilly Media & Associates,
;; Inc., Sebastopol, CA.

(defun maybe-switch-to-buffer (&rest args)
  "When interactive, switch to existing buffers only, unless
given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(defun maybe-pop-to-buffer (&rest args)
  "When interactive, pop to existing buffers only, unless given a
prefix argument."
  (interactive
   (list (read-buffer "Pop to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(advice-add 'switch-to-buffer :before #'maybe-switch-to-buffer)

(advice-add 'switch-to-buffer-other-window :before #'maybe-switch-to-buffer)

(advice-add 'switch-to-buffer-other-frame :before #'maybe-switch-to-buffer)

;; This one was not stolen, but the pattern is the same.
(advice-add 'pop-to-buffer :before 'maybe-pop-to-buffer)

(provide 'buffer-advice)
