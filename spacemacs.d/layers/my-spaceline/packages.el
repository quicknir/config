
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

  (defgroup nir-spaceline nil "")

  (defface nir-git-lines-added-face
    '((t (:foreground "green")))
    ""
    :group 'nir-spaceline)
  (defface nir-git-lines-added-face-icon
    '((t (:family "github-octicons" :foreground "green" )))
    ""
    :group 'nir-spaceline)

  (defface nir-git-lines-removed-face
    '((t (:foreground "red")))
    ""
    :group 'nir-spaceline)
  (defface nir-git-lines-modified-face
    '((t (:foreground "orange")))
    ""
    :group 'nir-spaceline)

  ;; (all-the-icons-octicon "diff-removed" :v-adjust 0.0)
  ;; (all-the-icons-octicon "diff-modified" :v-adjust 0.0)) diff-icons)
  (spaceline-define-segment nir-git
    "An `all-the-icons' segment to display Added/Removed stats for files under git VC."
    (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
      (let* ((space (propertize " " 'face `(:height ,(if spaceline-all-the-icons-slim-render 0.2 1.0))))
             (icons (list
                     (unless (zerop added)
                       (propertize (concat "+" (number-to-string added)) 'face 'nir-git-lines-added-face))
                     (unless (zerop removed)
                       (propertize (concat "-" (number-to-string removed)) 'face 'nir-git-lines-removed-face))
                     (unless (zerop modified)
                       (propertize (concat "*" (number-to-string modified)) 'face 'nir-git-lines-modified-face)))))
        (propertize
         (mapconcat 'identity (cl-remove-if 'not icons) " "))))
    :when (and active
               (not (equal '(0 0 0) (spaceline-all-the-icons--git-statistics)))))

  (set-fontset-font "fontset-default" '(61547 . 61547) (font-spec :name (all-the-icons-octicon-family)))

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
      ((all-the-icons-vc-icon
       all-the-icons-vc-status
       :separator "|" nir-git) :when active)
      ;;   ((all-the-icons-flycheck-status
      ;;     all-the-icons-flycheck-status-info) :separator " ")
      ;;   all-the-icons-package-updates)
      ;;  :face other-face
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
