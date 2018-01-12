(defun cpp2/rtags-find-symbol-at-point-other-window ()
  (interactive)
  (let((current-prefix-arg '(4)))
    (call-interactively 'rtags-find-symbol-at-point)
    ))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`cpp2-enable-clang-format-on-save' is non-nil."
  (when cpp2-enable-clang-format-on-save
    (clang-format-buffer)))

(defun spacemacs//add-clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`cpp2-enable-clang-format-on-save' is non-nil."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save))

(defun cpp2/find-other-file ()
  (interactive)
  (let ((ff-always-try-to-create nil)
        (ff-other-file-alist cpp2-other-file-alist))
    (ff-find-other-file nil t)))

(defun cpp2/find-other-file-other-window ()
  (interactive)
  (let ((ff-always-try-to-create nil)
        (ff-other-file-alist cpp2-other-file-alist))
    (ff-find-other-file t t)))

(defun cpp2/find-test-file ()
  (interactive)
  (let ((ff-always-try-to-create nil)
        (ff-other-file-alist cpp2-test-file-alist))
    (ff-find-other-file nil t)))

(defun cpp2/find-test-file-other-window ()
  (interactive)
  (let ((ff-always-try-to-create nil)
        (ff-other-file-alist cpp2-test-file-alist))
    (ff-find-other-file t t)))

(defun cpp2/goto-definition ()
  (interactive)
  (require 'rtags)
  (if (rtags-is-indexed) (rtags-find-symbol-at-point) (ycmd-goto)))
