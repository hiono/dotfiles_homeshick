;; auto completion like IntelliSense
(el-get-bundle! auto-complete
  (global-auto-complete-mode t)
  (setq ac-auto-show-menu 0.5)
  (let ((map ac-complete-mode-map))
    (define-key map (kbd "C-n") 'ac-next)
    (define-key map (kbd "C-p") 'ac-previous)
    (define-key map (kbd "TAB") nil)))

;; company
(el-get-bundle company-mode
  ;; permanently replace `company-complete-common' with
  ;; `company-complete-common-or-cycle'.
  (with-eval-after-load 'company
    (fset 'orig-company-complete-common
          (symbol-function 'company-complete-common))
    (defun company-complete-common ()
      (interactive)
      (eval-and-compile (require 'cl-lib))
      (cl-letf (((symbol-function 'company-complete-common)
                 (symbol-function 'orig-company-complete-common)))
        (call-interactively 'company-complete-common-or-cycle))))

  (with-eval-after-load-feature 'company
    (let ((map company-active-map))
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous)
      (define-key map (kbd "C-m") 'company-complete-selection))
    (set-face-background 'company-tooltip-selection "#4C7073")))
