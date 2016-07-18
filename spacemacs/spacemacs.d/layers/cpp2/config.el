;;; config.el --- cpp2 Layer config File for Spacemacs
;;

;; variables

(spacemacs|defvar-company-backends c-mode-common)
(spacemacs|defvar-company-backends cmake-mode)

(defvar cpp2-default-mode-for-headers 'c++-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")
