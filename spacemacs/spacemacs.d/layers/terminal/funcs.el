;; Callback for when user cuts
(defun xclip-cut-function (text &optional push)
  (let* ((process-connection-type nil)
         (proc (start-process "xclip" nil "xclip"
                              "-selection" "clipboard")))
    (process-send-string proc text)
    (process-send-eof proc)))

;; Call back for when user pastes
(defun xclip-paste-function()
  ;; Find out what is current selection by xclip. If it is different
  ;; from the top of the kill-ring (car kill-ring), then return
  ;; it. Else, nil is returned, so whatever is in the top of the
  ;; kill-ring will be used.
  (let ((xclip-output (shell-command-to-string "xclip -selection clipboard -o")))
    (unless (string= (car kill-ring) xclip-output)
      xclip-output)))

;; Make sure terminal client emacs has transparent background
(defun make-term-frame-trans (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-background 'font-lock-comment-face "unspecified-bg" frame)))
