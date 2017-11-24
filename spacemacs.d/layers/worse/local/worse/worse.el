;;; -*- lexical-binding: t -*-

(require 'projectile)
(require 'helm-ag)
(require 'helm)
(require 'helm-help)
(require 'helm-projectile)
(require 'helm-source)

(defvar worse-projectile-files-buffer "*worse files*")
(defvar worse-base-command '("ag" "--nocolor" "--nogroup" "-S" "-g"))

(defun worse--construct-ag-command (pattern)
  (let* ((opt-query (helm-ag--parse-options-and-query pattern))
         (options (car opt-query))
         (query (cdr opt-query))
         (has-query (not (string= query ""))))
    (when (helm-ag--show-result-p options has-query)
      (append worse-base-command
              options
              (and has-query (list (helm-ag--join-patterns query)))))))

(setq worse-helm-source-projectile-files-list
     (helm-build-async-source "Projectile files"
       :candidates-process
       (lambda ()
         (apply 'start-process "ag-files" nil (worse--construct-ag-command helm-pattern)))
       :keymap helm-projectile-find-file-map
       :help-message 'helm-ff-help-message
       :mode-line "foo"
       :action helm-projectile-file-actions
       :persistent-action #'helm-projectile-file-persistent-action
       :persistent-help "Preview file"
       :requires-pattern 3))

;;;###autoload
(defun worse-faster-projectile-find-file (input)
  (condition-case nil
      (let* ((default-directory (projectile-project-root))
             (output (apply 'process-lines (worse--construct-ag-command-2 (concat "-Q -g " input)))))
        (if (cdr output)
            (worse-faster-projectile-files-input input)
          (find-file-existing (concat default-directory (car output)))))
    (error (message "No files found or error running process"))))

;;;###autoload
(defun worse-faster-projectile-files ()
 (interactive)
 (let ((default-directory (projectile-project-root)))
   (helm :sources 'worse-helm-source-projectile-files-list
         :buffer "*helm worse files*")))

;;;###autoload
(defun worse-faster-projectile-files-input (input)
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (helm :sources 'worse-helm-source-projectile-files-list
          :buffer "*helm worse files*"
          :input input)))


(provide 'worse)
