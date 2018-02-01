
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

  (spaceline-define-segment nir-git
    "An `all-the-icons' segment to display Added/Removed stats for files under git VC."
    (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
      (cl-destructuring-bind (added-icon removed-icon modified-icon) (spaceline-all-the-icons-icon-set-git-stats)
        (let* ((space (propertize " " 'face `(:height ,(if spaceline-all-the-icons-slim-render 0.2 1.0))))
               (icons (list
                       (spaceline-all-the-icons--git-stats added-icon added 'success)
                       (unless (zerop removed) (spaceline-all-the-icons--git-stats removed-icon removed 'error))
                       (unless (zerop modified) (spaceline-all-the-icons--git-stats modified-icon modified 'warning)))))
          ;; (propertize
          ;;  (mapconcat 'identity (cl-remove-if 'not icons) space))
          (propertize  (car icons))
          )))

    :when (and active
               (not (equal '(0 0 0) (spaceline-all-the-icons--git-statistics)))))
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-toggle-all-the-icons-git-ahead-on)
  (spaceline-compile
    `(((persp-name
        workspace-number
        window-number)
       :fallback evil-state
       :face highlight-face
       :priority 0)
      (anzu :priority 4)
      ((buffer-modified buffer-size buffer-id remote-host all-the-icons-mode-icon)
       :priority 5)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 3)
      (minor-modes :when active)
      (erc-track :when active)
      (all-the-icons-vc-icon
       all-the-icons-vc-status)
      ((all-the-icons-git-ahead
       all-the-icons-git-status) :separation " ")
      ;; ((all-the-icons-vc-icon
      ;;   all-the-icons-vc-status
      ;;   ((all-the-icons-git-ahead
      ;;     all-the-icons-git-status) :separator " ")
      ;;   ((all-the-icons-flycheck-status
      ;;     all-the-icons-flycheck-status-info) :separator " ")
      ;;   all-the-icons-package-updates)
      ;;  :face other-face
      ;;  :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-secondary-separator " "))
      (version-control :when active
                       :priority 7)
      (org-pomodoro :when active)
      (org-clock :when active))
    `(which-function
      (python-pyvenv :fallback python-pyenv)
      (selection-info :priority 2)
      ((point-position
        line-column)
       :separator " | "
       :priority 3)
      (global :when active)
      (buffer-position :priority 0)
      (hud :priority 0)))
  )
