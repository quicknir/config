
(setq my-spaceline-packages
      '(
        all-the-icons
        spaceline-all-the-icons
        ))

(defun my-spaceline/init-all-the-icons ()
  (require 'all-the-icons))

(defun my-spaceline/init-spaceline-all-the-icons ()
  (require 'spaceline)
  (require 'spaceline-all-the-icons)

  (spaceline-define-segment evil-mc-segment
    (let ((face (if (evil-mc-frozen-p) 'error 'success)))
      (propertize (concat "Ⓔ " (format "%d" (evil-mc-get-cursor-count))) 'face face)
      )
    :when (and active (> (evil-mc-get-cursor-count) 1))
    )

  (spaceline-define-segment ycmd-segment
    (pcase ycmd--last-status-change
      (`parsed (propertize  "Ⓨ" 'face 'success))
      (`parsing (propertize  "Ⓨ " 'face 'warning))
      (`unparsed (propertize  "Ⓨ ?" 'face 'error))
      (`stopped (propertize  "Ⓨ -" 'face 'error))
      (`starting (propertize  "Ⓨ >" 'face 'error))
      (`errored (propertize  "Ⓨ !" 'face 'error)))
    :when (and active (bound-and-true-p ycmd-mode))
    )

  (spaceline-define-segment nir-git
    (progn
      (vc-state-refresh (buffer-file-name) 'Git)
      (let ((prefix (concat " " (car (vc-git-branches)))))
        (pcase (vc-state (buffer-file-name))
          (`edited
           (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
             (let* ((icons
                     (list
                      " |"
                      (propertize (concat "+" (number-to-string added)) 'face 'success)
                      (propertize (concat "-" (number-to-string removed)) 'face 'error)
                      (propertize (concat "*" (number-to-string modified)) 'face 'warning))))
               (concat prefix (propertize
                               (mapconcat 'identity icons " "))))))
          (`up-to-date prefix)
          (`added "added")
          (_ "shit see what's going on")
          )))
    :when (and active vc-mode (buffer-file-name)))

  ;; (defun spaceline-all-the-icons--flycheck-status ()
  ;;   "Render the mode line for Flycheck Status in a more verbose fashion."
  ;;   (let* ((text (cl-case flycheck-last-status-change
  ;;                  (finished    (spaceline-all-the-icons--flycheck-finished))
  ;;                  (running     (concat (all-the-icons-faicon "refresh") " Running"))
  ;;                  (no-checker  "⚠ No Checker")
  ;;                  (not-checked "✖ Disabled")
  ;;                  (errored     "⚠ Error")
  ;;                  (interrupted "⛔ Interrupted")))

  (defun my-spaceline/flycheck-one-type (counts state prefix face)
    (let* ((errorp (flycheck-has-current-errors-p state))
           (err (cdr (assq state counts))))
      (if errorp
          (propertize (concat prefix (if err (number-to-string err) "?")) 'face face))))

  (defun my-spaceline/flycheck-counts ()
    (if (not (flycheck-has-current-errors-p)) (propertize "✔" 'face 'success)
      (let* ((counts (flycheck-count-errors flycheck-current-errors)))
        (concat
         (my-spaceline/flycheck-one-type counts 'error "✖" 'spaceline-flycheck-error)
         (my-spaceline/flycheck-one-type counts 'warning "⚠" 'spaceline-flycheck-warning)
         (my-spaceline/flycheck-one-type counts 'info "🛈" 'spaceline-flycheck-info))
        )))

  (spaceline-define-segment my-flycheck
    (pcase  flycheck-last-status-change
      ('finished    (my-spaceline/flycheck-counts))
      ('running     "")
      ('no-checker  (propertize "⚠ No Checker" 'face 'error))
      ('not-checked (propertize "✖ Disabled" 'face 'error))
      ('errored     (propertize "⚠ Error" 'face 'error))
      ('interrupted (propertize "⛔ Interrupted" 'face 'error)))
    :when (and active (bound-and-true-p flycheck-mode))
    )

  (if (display-graphic-p)
      (setq-default powerline-default-separator 'arrow)
    (setq-default powerline-default-separator 'utf-9))

  ;; Minor modes mostly not useful; have special segments for ycmd, evil-mc
  ;; But leave it in so it's easy to toggle back on
  (spaceline-toggle-minor-modes-off)

  (spaceline-compile
    `(
      ((persp-name
        workspace-number
        window-number)
       :fallback evil-state
       :face highlight-face
       :separator "|"
       :priority 0)
      ((buffer-modified buffer-size buffer-id remote-host all-the-icons-mode-icon)
       :priority 5)
      (process :when active)
      (minor-modes :when active)
      (erc-track :when active)
      nir-git
      ycmd-segment
      my-flycheck
      evil-mc-segment
      (org-pomodoro :when active)
      (org-clock :when active)
      (all-the-icons-multiple-cursors)
      )
    `(
      (anzu :priority 4)
      which-function
      (python-pyvenv :fallback python-pyenv)
      (selection-info :priority 2)
      ((point-position
        line-column)
       :separator " | "
       :priority 3)
      (global :when active)
      (buffer-position :priority 0)
      (hud :priority 0)
      )
    ))
