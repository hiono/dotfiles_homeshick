(setq-default magit-diff-refine-hunk 'all
              magit-display-buffer-function
              #'(lambda (buffer)
                  (display-buffer buffer '(display-buffer-same-window)))
              magit-auto-revert-mode nil)
(el-get-bundle (magit magit-gitflow)
  (with-eval-after-load-feature 'magit-diff
    (let ((map magit-diff-mode-map))
      (define-key map (kbd "RET") 'magit-ediff-dwim)
      (define-key map "v" 'magit-diff-visit-file)
      (define-key map (kbd "M-.") 'magit-diff-visit-file)
      (define-key map (kbd "SPC") 'magit-jump-to-diffstat-or-diff)))
  (with-eval-after-load-feature 'magit-section
    (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer))
  (with-eval-after-load-feature (magit-mode)
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))
