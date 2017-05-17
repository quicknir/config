(setq pretty-git-modeline-packages '(spaceline))

(defun pretty-git-modeline/post-init-spaceline ()
  (with-eval-after-load 'spaceline
    (spaceline-define-segment version-control
       "Version control information."
       (when vc-mode
         (powerline-raw
          (replace-regexp-in-string "Git." ":"
                                    (s-trim (concat vc-mode
                                                    (when (buffer-file-name)
                                                      (pcase (vc-state (buffer-file-name))
                                                        (`up-to-date "")
                                                        (`edited "*")
                                                        (`added "+")
                                                        (`unregistered "?")
                                                        (`removed "-")
                                                        (`needs-merge "!")
                                                        (`needs-update " Upd")
                                                        (`ignored " Ign")
                                                        (_ "?")))))))))))
