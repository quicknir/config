(setq terminal-packages '(xclip))

(defun terminal/init-xclip ()
  (require 'xclip)
  (xclip-mode 1)
  (turn-on-xclip))
