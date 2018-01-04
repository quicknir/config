(setq terminal-packages '(xclip))

(defun terminal/init-xclip ()
  (use-package xclip
    :config
      (xclip-mode 1)))
