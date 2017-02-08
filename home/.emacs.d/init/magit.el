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
  (with-eval-after-load-feature 'magit-mode
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
  (with-eval-after-load-feature 'magit-diff
    ;; cursorが載っていない状態でのaddedのface
    (set-face-foreground 'magit-diff-added "#00FF00")
    (set-face-background 'magit-diff-added "#000000")
    ;; cursorが載っている状態のaddedのface
    (set-face-foreground 'magit-diff-added-highlight "#00FF00")
    (set-face-background 'magit-diff-added-highlight "gray20")
    ;; cursor載っていない状態のremoved
    (set-face-foreground 'magit-diff-removed "#FF0000")
    (set-face-background 'magit-diff-removed "#000000")
    ;; cursor載っている状態のremoved
    (set-face-foreground 'magit-diff-removed-highlight "#FF0000")
    (set-face-background 'magit-diff-removed-highlight "gray20")
    ;; lineを選択してstageしようとするときのface
    (set-face-background 'magit-diff-lines-boundary "blue")
    ;;(set-face-foreground 'magit-hash "red")
    )
)
