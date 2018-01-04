
(setq cpp2-packages
  '(
    cc-mode
    clang-format
    cmake-mode
    company
    company-ycmd
    flycheck
    gdb-mi
    helm-rtags
    modern-cpp-font-lock
    rtags
    ycmd
    ))

(defun cpp2/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h\\'" . ,cpp2-default-mode-for-headers))
    :config
    (progn
      (require 'compile)
      (dolist (mode '(c-mode c++-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "ga" 'cpp2/find-other-file
          "gA" 'cpp2/find-other-file-other-window
          "gt" 'cpp2/find-test-file
          "gT" 'cpp2/find-test-file-other-window)))))

(defun cpp2/init-clang-format ()
  (use-package clang-format)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    "=" 'clang-format-buffer))

(defun cpp2/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))))

(defun cpp2/post-init-company ()
  (spacemacs|add-company-backends
    :modes c++-mode)
  (spacemacs|add-company-backends
    :backends company-cmake
    :modes cmake-mode))

(defun cpp2/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/enable-flycheck mode)))

(defun cpp2/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t))
  :config
  (progn
    (require 'compile)
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "dd" 'gdb
        "dc" 'gud-cont
        "dn" 'gud-next
        "ds" 'gud-step
        "db" 'gud-break
        "dB" 'gud-remove
        "dr" 'gud-go
        "da" 'gdb-io-eof
        "dk" 'gud-up
        "dj" 'gud-down
        "du" 'gud-until
        ))))

(defun cpp2/init-helm-rtags ()
    (use-package helm-rtags
      :defer t
      :init (setq rtags-display-result-backend 'helm)))

(defun cpp2/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    :config
    (spacemacs|diminish modern-c++-font-lock-mode)
    ))

(defun cpp2/init-rtags ()
  (use-package rtags
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "gg" 'cpp2/goto-definition
        "/" 'rtags-find-symbol
        "gr" 'rtags-find-references-at-point
        "v" 'rtags-find-virtuals-at-point
        "i" 'rtags-imenu
        "rr" 'rtags-rename-symbol

        ;; print prefix
        "pt" 'rtags-print-class-hierarchy
        "pe" 'rtags-print-enum-value-at-point
        "pi" 'rtags-print-dependencies
        "ps" 'rtags-print-symbol-info
        "pp" 'rtags-preprocess-file
        )
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (add-to-list 'spacemacs-jump-handlers-c++-mode '(cpp2/goto-definition :async t)))
    :config
    (progn
      (setq rtags-jump-to-first-match nil))))

(defun cpp2/post-init-ycmd ()
  (spacemacs/add-to-hooks 'ycmd-mode '(c++-mode-hook c-mode-hook))
  (setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
  (setq ycmd-idle-change-delay 2.0)
  (setq ycmd-confirm-fixit nil)
  (setq ycmd-global-config
        (concat (configuration-layer/get-layer-path 'cpp2)
                "ycmd_global_conf.py"))
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    "F" 'ycmd-fixit
    "pS" 'ycmd-get-type))

(defun cpp2/post-init-company-ycmd ()
  (spacemacs|add-company-backends :backends company-ycmd :modes c-mode-common))
