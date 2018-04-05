(setq evil-mini-packages
      '(
        evil
        ))

(defun evil-mini/post-init-evil ()
  "Initialize minibuffer for `evil'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
  ;; WARNING: With lexical binding, lambdas from `mapc' and `dolist' become
  ;; closures in which we must use `evil-define-key*' instead of
  ;; `evil-define-key'.
  (dolist (map (list minibuffer-local-map
                     minibuffer-local-ns-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-isearch-map))
    (evil-define-key* 'normal map (kbd "<escape>") 'abort-recursive-edit)
    (evil-define-key* 'normal map (kbd "<return>") 'exit-minibuffer)
    (evil-define-key* 'insert map (kbd "C-r") 'helm-minibuffer-history))

  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  ;; Because of the above minibuffer-setup-hook, some evil-ex bindings need be reset.
  (evil-define-key 'normal evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-p") 'previous-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-n") 'next-history-element)
  (with-eval-after-load 'helm
    (progn
      (evil-define-key 'insert helm-map (kbd "C-k") 'helm-previous-line)
      (evil-define-key 'insert helm-map (kbd "C-z") 'helm-select-action)
      ))
  )
