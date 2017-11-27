
(defun mc-column--col-at-point (point)
  (save-excursion (goto-char point) (current-column)))

(defun mc-column--make-cursor-at-col-append (_startcol endcol orig-line)
  (end-of-line)
  (when (> endcol (current-column))
    (insert-char ?\s (- endcol (current-column))))
  (move-to-column (- endcol 1))
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here)))

(defun mc-column--make-cursor-at-col-insert (startcol _endcol orig-line)
  (end-of-line)
  (move-to-column startcol)
  (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
    (evil-mc-make-cursor-here)))

(defun mc-column--make-vertical-cursors (beg end func)
  (evil-mc-pause-cursors)
  (apply-on-rectangle func
                      beg end (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-normal-state))

(defun evil-mc-insert-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (mc-column--make-vertical-cursors beg end 'mc-column--make-cursor-at-col-insert)
  (move-to-column (min (mc-column--col-at-point beg) (mc-column--col-at-point end))))

(defun evil-mc-append-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (let ((final-column (- (max (mc-column--col-at-point beg) (mc-column--col-at-point end)) 1)))
    (mc-column--make-vertical-cursors beg end 'mc-column--make-cursor-at-col-append)
    (mc-column--col-at-point beg)
    (move-to-column final-column)))
