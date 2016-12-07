;; hostname
(defconst short-hostname (or (nth 0 (split-string (system-name) "\\."))
                             (system-name))
  "Host part of function `system-name'.")

;; shell
(setq-default explicit-shell-file-name "fish")
(setq shell-file-name "fish"
      shell-command-switch "-c")

;; interactive
(fset 'yes-or-no-p 'y-or-n-p)

;; replace 'ls'
(load "ls-lisp")
(let (current-load-list)
  (defadvice insert-directory
      (around reset-locale activate compile)
    (let ((system-time-locale "C"))
      ad-do-it)))
;; highlight today modified files
(defface todays-face-f '((t (:foreground "GreenYellow"))) nil)
(defvar todays-face-f 'todays-face-f)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat (format-time-string "%b %e" (current-time)) " [0-9]....") arg t))
(add-hook 'dired-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              major-mode
              (list
               '(my-dired-today-search . todays-face-f)
               ))))

;; delete trailing whitespace
(defvar my:delete-trailing-whitespace-exclude-suffix
  (list "\\.rd$" "\\.md$" "\\.rbt$" "\\.rab$"))
(defun my:delete-trailing-whitespace ()
  (interactive)
  (cond
   ((equal nil
           (loop for pattern in my:delete-trailing-whitespace-exclude-suffix
                 thereis (string-match pattern buffer-file-name)))
    (delete-trailing-whitespace))))
(add-hook 'before-save-hook 'my:delete-trailing-whitespace)
