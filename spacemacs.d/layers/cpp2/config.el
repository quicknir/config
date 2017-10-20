;;; config.el --- cpp2 Layer config File for Spacemacs
;;

;; variables


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
