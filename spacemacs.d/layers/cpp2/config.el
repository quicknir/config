;;; config.el --- cpp2 Layer config File for Spacemacs
;;

;; variables


(defconst cpp2-modes '(c-mode c++-mode)
  "Primary major modes of the `cpp2' layer.")

(defconst cpp2-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `cpp2' layer.")

(defvar cpp2-default-mode-for-headers 'c++-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(defvar cpp2-test-file-alist
  '(
    ("\\.cpp\\'" (".t.cpp"))
    ("\\.h\\'" (".t.cpp"))
    ))

(defvar cpp2-other-file-alist
  '(
    ("\\.t\\.cpp\\'" (".h" ".cpp"))
    ("\\.h\\'"   (".cpp" ".c" ".cc"))
    ("\\.cpp\\'" (".h" ".hpp"))

    ("\\.c\\'"   (".h"))
    ("\\.hpp\\'" (".cpp"))
    ))

(spacemacs|define-jump-handlers c++-mode)

(defvar cpp2-enable-clang-format-on-save nil
  "If non-nil, automatically format code with clang-format on
  save)"
)

(defvar cpp2-default-style
  '("stroustrup"
    (indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c-offsets-alist . (
                        (innamespace . -)
                        ))
    ))
