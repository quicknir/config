;; Useful random bits of stuff I want to keep handy. Nicer to keep in a separate
;; file than commented out in my .spacemacs

  ;; (flycheck-define-checker
  ;;     python-mypy ""
  ;;     :command ("mypy"
  ;;               "--ignore-missing-imports" "--fast-parser"
  ;;               "--python-version" "3.6"
  ;;               source-original)
  ;;     :error-patterns
  ;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
  ;;     :modes python-mode)

  ;; (add-to-list 'flycheck-checkers 'python-mypy t)
  ;; (flycheck-add-next-checker 'python-pylint 'python-mypy t)

;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame t
;;       helm-use-undecorated-frame-option t)

;; (defgroup nir-spaceline nil "")

;; (defface nir-git-lines-added-face
;;   '((t (:foreground "green")))
;;   ""
;;   :group 'nir-spaceline)

;; (all-the-icons-octicon "diff-removed" :v-adjust 0.0)
;; (all-the-icons-octicon "diff-modified" :v-adjust 0.0)) diff-icons)

;; (set-fontset-font "fontset-default" '(61547 . 61547)
;;                   (font-spec :name (all-the-icons-octicon-family)))
