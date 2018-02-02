
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
      (let* ((icons
              (list
               (propertize (concat "+" (number-to-string added)) 'face 'success)
               (propertize (concat "-" (number-to-string removed)) 'face 'error)
               (propertize (concat "*" (number-to-string modified)) 'face 'warning))))

        (propertize
         (mapconcat 'identity icons " "))))
    :when (and active
               (not (equal '(0 0 0) (spaceline-all-the-icons--git-statistics)))))

  (setq powerline-default-separator 'arrow)

  (spaceline-compile
    `(((persp-name
        workspace-number
        window-number)
       :fallback evil-state
       :face highlight-face
       :priority 0)
      ((buffer-modified buffer-size buffer-id remote-host all-the-icons-mode-icon)
       :priority 5)
      (process :when active)
      (minor-modes :when active)
      (erc-track :when active)
      ((all-the-icons-vc-icon
        all-the-icons-vc-status
        :separator "|" nir-git) :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 3)
      (org-pomodoro :when active)
      (org-clock :when active)
      )
    `
    (
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
     (hud :priority 0)))
  )
