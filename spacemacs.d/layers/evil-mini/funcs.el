
(defun evil-collection-minibuffer-insert ()
  "Switch to insert state.
This function is meant to be hooked in the minibuffer:
  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
`evil-set-initial-state' can not be used for the minibuffer since
it does not have a mode."
  (set (make-local-variable 'evil-echo-state) nil)
  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
  ;; way to do this, but the minibuffer doesn't have a mode.
  ;; The alternative is to create a minibuffer mode (here), but
  ;; then it may conflict with other packages' if they do the same.
  (evil-insert 1))
