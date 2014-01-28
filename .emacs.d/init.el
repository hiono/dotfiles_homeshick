;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 言語を日本語にする
(set-language-environment 'Japanese)
(setenv "LC_ALL" "C")
(setenv "LC_TIME" "C")
(global-set-key (kbd "C-\\") nil)

; 極力 UTF-8 とする
(prefer-coding-system 'utf-8)

;;; load-path を設定する
(let ((default-directory (expand-file-name "~/.emacs.d/")))
  ;(add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 『 dired http://homepage1.nifty.com/blankspace/emacs/dired.html 』
; dired関連
(require 'ls-lisp)
(let (current-load-list)
  (defadvice insert-directory
    (around reset-locale activate compile)
    (let ((system-time-locale "C"))
      ad-do-it)))
(setq ls-lisp-dirs-first t)
(require 'wdired)
;; 本日変更のファイル
;; 色が付けられる環境なら、今日変更されたファイルが一目で
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 『 emacs - Should `flet` be replaced with `cl-flet` or `cl-letf` ? - Stack Overflow http://stackoverflow.com/questions/18895605/should-flet-be-replaced-with-cl-flet-or-cl-letf 』
(defmacro lawlist-flet (bindings &rest body)
      "Make temporary overriding function definitions.
    This is an analogue of a dynamically scoped `let' that operates on the function
    cell of FUNCs rather than their value cell.
    If you want the Common-Lisp style of `flet', you should use `cl-flet'.
    The FORMs are evaluated with the specified function definitions in place,
    then the definitions are undone (the FUNCs go back to their previous
    definitions, or lack thereof).
    \(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
      (declare (indent 1) (debug cl-flet)
    ;;           (obsolete "use either `cl-flet' or `cl-letf'."  "24.3")
                    )
      `(letf ,(mapcar
               (lambda (x)
                 (if (or (and (fboundp (car x))
                              (eq (car-safe (symbol-function (car x))) 'macro))
                         (cdr (assq (car x) macroexpand-all-environment)))
                     (error "Use `labels', not `flet', to rebind macro names"))
                 (let ((func `(cl-function
                               (lambda ,(cadr x)
                                 (cl-block ,(car x) ,@(cddr x))))))
                   (when (cl--compiling-file)
                     ;; Bug#411.  It would be nice to fix this.
                     (and (get (car x) 'byte-compile)
                          (error "Byte-compiling a redefinition of `%s' \
    will not work - use `labels' instead" (symbol-name (car x))))
                     ;; FIXME This affects the rest of the file, when it
                     ;; should be restricted to the flet body.
                     (and (boundp 'byte-compile-function-environment)
                          (push (cons (car x) (eval func))
                                byte-compile-function-environment)))
                   (list `(symbol-function ',(car x)) func)))
               bindings)
         ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 『 Emacs でプラグインを簡単にインストールするための package.el を使ってみる - おんがえしの日記 http://d.hatena.ne.jp/tuto0621/20120613/1339607400 』
;(setq byte-compile-warnings '(not obsolete))
(require 'package)
; Add package-archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

; Initialize
(package-initialize)

(require 'cl)
(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    ;; melpa ;; 2014-01-07辺りからErrorになったので外し
    magit
    gist
    markdown-mode
    google-c-style
    yaml-mode
    open-junk-file
    helm
    ace-jump-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

; melpa.el (download後しか使えないからここに書く)
;;(require 'melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Magit
(require 'magit)
(define-key global-map (kbd "C-x m") 'magit-status)
(define-key magit-mode-map (kbd "C-x l") 'magit-log)
;; ;; for git-redmine
;; (magit-define-inserter redmine ()
;;   (magit-git-section 'redmine
;;                      "Redmine:" 'redmine-tickets-function
;;                      "redmine" "arg1"))
;; (add-hook 'magit-before-stashes-hook 'magit-insert-redmine)
;; (defun redmine-tickets-function ()
;;   (let ((redmine (buffer-substring (line-beginning-position) (line-end-position))))
;;     (goto-char (line-beginning-position))
;;     (magit-with-section redmine 'redmine
;;       (magit-set-section-info redmine)
;;       (forward-line))))

(defun magit-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options '("-b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gist
(require 'magit)
(require 'gist)
(setq gist-view-gist t)
(setq github-user (magit-get "github.user"))
(setq github-password (magit-get "github.password"))
(setq github-token (magit-get "github.token"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
(global-set-key (kbd "C-x j") 'open-junk-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; aliases
(defalias 'qrr 'query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helm
;  おすすめの helm拡張 - Life is very short http://d.hatena.ne.jp/syohex/20121207/1354885367
(require 'helm)
(global-set-key (kbd "C-c r") `helm-recentf)
(global-set-key (kbd "M-y") `helm-show-kill-ring)
;; helm-git-project
(defun helm-c-sources-git-project-for (pwd)
  (loop for elt in
	'(("Modified files" . "--modified")
	  ("Untracked files" . "--others --exclude-standard")
	  ("All controlled files in this project" . nil))
	for title  = (format "%s (%s)" (car elt) pwd)
	for option = (cdr elt)
	for cmd    = (format "git ls-files %s" (or option ""))
	collect
	`((name . ,title)
	  (init . (lambda ()
		    (unless (and (not ,option) (helm-candidate-buffer))
		      (with-current-buffer (helm-candidate-buffer 'global)
			(call-process-shell-command ,cmd nil t nil)))))
	  (candidates-in-buffer)
	  (type . file))))

(defun helm-git-project-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-git-project ()
  (interactive)
  (let ((topdir (helm-git-project-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let* ((default-directory topdir)
	   (sources (helm-c-sources-git-project-for default-directory)))
            (helm-other-buffer sources "*helm git project*"))))
(global-set-key (kbd "C-h g") `helm-git-project)

;; fix code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; debian-wheezy等ではemacs24に問題あるため
;; (defadvice package--add-to-archive-contents
;;   (around package-filter-add-to-archive-contents (package archive)
;;           activate compile)
;;   "Add filtering of available packages using `package-filter-function', if non-nil."
;;   (when (and package-filter-function
;;              (funcall package-filter-function
;;                       (car package)
;;                     (or (ignore-errors ; < 24.3.50
;;                           (package-desc-vers (cdr package)))
;;                         (ignore-errors ; >= 24.3.50
;;                           (package-desc-version (cdr package))))
;;                       archive))
;;     ad-do-it))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loaded")
