(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  ;; See http://stackoverflow.com/questions/7410125
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

(use-package
 popper
 :ensure t
 :bind
 (("C-`" . popper-toggle)
  ("M-`" . popper-cycle)
  ("C-M-`" . popper-toggle-type))
 :init
 (setq popper-reference-buffers
       '("\\*Messages\\*"
         "\\*vterm\\*$"
         "\\*IPython\\*$"
         "Output\\*$"
         "\\*Async Shell Command\\*"
         help-mode
         compilation-mode))
 (popper-mode +1) (popper-echo-mode +1))
