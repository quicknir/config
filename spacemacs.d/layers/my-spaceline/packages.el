
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

  ;; Use this to define evil-mc segment
  ;; (defun evil-mc-active-mode-line (prefix)
  ;;   "Get the mode-line text to be displayed when there are active cursors"
  ;;   (let ((mode-line-text
  ;;          (concat mode-line-text-prefix
  ;;                  (when (and (evil-mc-frozen-p)
  ;;                             evil-mc-mode-line-text-paused)
  ;;                    "(paused)")
  ;;                  (format ":%d" (evil-mc-get-cursor-count)))))
  ;;     ;; mode line text colors
  ;;     (cond ((and (evil-mc-frozen-p)
  ;;                 evil-mc-mode-line-text-inverse-colors)
  ;;            (propertize mode-line-text 'face '(:inverse-video t)))
  ;;           ;; resumed (unfrozen) cursors
  ;;           (evil-mc-mode-line-text-cursor-color
  ;;            (propertize
  ;;             mode-line-text
  ;;             'face
  ;;             '(:inherit cursor :foreground "black" :distant-foreground "white")))
  ;;           ;; default colors
  ;;           (t mode-line-text))))

  ;; Use this to define ycmd segment (then remove minor mode segment, or at least toggle off by default)
  ;; (defun ycmd--mode-line-status-text ()
  ;;   "Get text for the mode line."
  ;;   (let ((force-semantic
  ;;          (when ycmd-force-semantic-completion "/s"))
  ;;         (text (pcase ycmd--last-status-change
  ;;                 (`parsed "")
  ;;                 (`parsing "*")
  ;;                 (`unparsed "?")
  ;;                 (`stopped "-")
  ;;                 (`starting ">")
  ;;                 (`errored "!"))))
  ;;     (concat " ycmd" force-semantic text)))

  (spaceline-define-segment nir-git
    (progn
      (vc-refresh-state)
      (pcase (vc-state (buffer-file-name))
        (`edited
         (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
           (let* ((icons
                   (list
                    "|"
                    (propertize (concat "+" (number-to-string added)) 'face 'success)
                    (propertize (concat "-" (number-to-string removed)) 'face 'error)
                    (propertize (concat "*" (number-to-string modified)) 'face 'warning))))

             (propertize
              (mapconcat 'identity icons " ")))))
        (`added "added")
        (_ "shit see what's going on")
        ))

    :when (and active vc-mode (buffer-file-name)
               (not (equal (vc-state (buffer-file-name)) `up-to-date)))
    ;; :when active
    )

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
        nir-git) :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 3)
      (org-pomodoro :when active)
      (org-clock :when active)
      (all-the-icons-multiple-cursors)
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
