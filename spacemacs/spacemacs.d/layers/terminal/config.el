(unless window-system
  (setq interprogram-cut-function 'xclip-cut-function)
  (setq interprogram-paste-function 'xclip-paste-function))
;; Idea from
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html)

(add-hook 'after-make-frame-functions 'make-term-frame-trans)
