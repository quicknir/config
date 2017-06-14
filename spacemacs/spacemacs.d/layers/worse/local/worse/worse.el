;;; -*- lexical-binding: t -*-

(require 'projectile)
(require 'helm-ag)
(require 'helm)
(require 'helm-help)
(require 'helm-projectile)
(require 'helm-source)

(defvar worse-projectile-files-buffer "*worse files*")

(setq worse-helm-source-projectile-files-list
     (helm-build-async-source "Projectile files"
       :candidates-process
       (lambda ()
         (apply 'start-process "ag-files" nil (helm-ag--construct-do-ag-command (concat "-g -S " helm-pattern))))
       :keymap helm-projectile-find-file-map
       :help-message 'helm-ff-help-message
       :mode-line "foo"
       :action helm-projectile-file-actions
       :persistent-action #'helm-projectile-file-persistent-action
       :persistent-help "Preview file"
       :requires-pattern 3))

;;;###autoload
(defun worse-faster-projectile-files ()
 (interactive)
 (let ((default-directory (projectile-project-root)))
   (helm :sources 'worse-helm-source-projectile-files-list
         :buffer "*helm worse files*")))

(provide 'worse)
