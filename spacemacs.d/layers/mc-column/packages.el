(setq mc-column-packages '(evil-mc))

(defun mc-column/post-init-evil-mc()
  (evil-define-key 'visual global-map "gI" 'evil-mc-insert-vertical-cursors)
  (evil-define-key 'visual global-map "gA" 'evil-mc-append-vertical-cursors)
  )
