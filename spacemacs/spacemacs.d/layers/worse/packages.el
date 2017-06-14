(setq worse-packages
      '(
        (worse :location local)))

(defun worse/init-worse ()
  (use-package worse
    :init (spacemacs/set-leader-keys "pf" 'worse-faster-projectile-files)))
