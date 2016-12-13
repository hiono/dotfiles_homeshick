;; basic key bindings
(global-set-key (kbd "M-<down>") #'enlarge-window)
(global-set-key (kbd "M-<up>") #'shrink-window)

;; terminal fix
(global-set-key (kbd "ESC <down>") #'scroll-other-window)
(global-set-key (kbd "ESC <up>") #'scroll-other-window-down)

;; package key bindings
(global-set-key (kbd "C-x m") 'magit-status)
