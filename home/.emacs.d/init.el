;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語を日本語にする
(set-language-environment 'Japanese)
(setenv "LC_ALL" "C")
(setenv "LC_TIME" "C")

;; disable default-keys
(define-key global-map (kbd "C-\\") nil)
(define-key global-map (kbd "C-x m") nil)

;; 極力 UTF-8 とする
(prefer-coding-system 'utf-8)

;;; load-path を設定する
(let ((default-directory (expand-file-name "~/.emacs.d/")))
  ;(add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired http://homepage1.nifty.com/blankspace/emacs/dired.html
;; dired関連
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
;; emacs - Should `flet` be replaced with `cl-flet` or `cl-letf` ? - Stack Overflow http://stackoverflow.com/questions/18895605/should-flet-be-replaced-with-cl-flet-or-cl-letf
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
                          (error "Byte-compiling a redefinition of `%s' will not work - use `labels' instead" (symbol-name (car x))))
                     ;; FIXME This affects the rest of the file, when it
                     ;; should be restricted to the flet body.
                     (and (boundp 'byte-compile-function-environment)
                          (push (cons (car x) (eval func))
                                byte-compile-function-environment)))
                   (list `(symbol-function ',(car x)) func)))
               bindings)
         ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Emacs でプラグインを簡単にインストールするための package.el を使ってみる - おんがえしの日記 http://d.hatena.ne.jp/tuto0621/20120613/1339607400
;(setq byte-compile-warnings '(not obsolete))
(require 'package)
;; Add package-archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Initialize
(package-initialize)

(require 'cl)
(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    ;; melpa ;; 2014-01-07辺りからErrorになったので外し
    magit
    magit-gitflow
    gist
    markdown-mode
    google-c-style
    yaml-mode
    open-junk-file
    ace-jump-mode
    ag
    wgrep-ag
    adoc-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;; melpa.el (download後しか使えないからここに書く)
;;(require 'melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Magit
(require 'magit)
;; (setq magit-diff-options '("-b"))
(define-key global-map (kbd "C-h m") 'magit-status)
(define-key global-map (kbd "C-x m") 'magit-status)
(eval-after-load 'magit
  '(progn
     (define-key magit-mode-map (kbd "C-h") 'delete-backward-char)
     (set-face-background 'magit-diff-add "#222222")
     (set-face-background 'magit-diff-del "#222222")
     (set-face-foreground 'magit-diff-file-header "#0000ff")
     (set-face-background 'magit-diff-file-header "#888888")
     ))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Magit-GitFlow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gist
(require 'gist)
(setq gist-view-gist t)
(setq github-user (magit-get "github.user"))
(setq github-password (magit-get "github.password"))
(setq github-token (magit-get "github.token"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
(define-key global-map (kbd "C-x j") 'open-junk-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aliases
(defalias 'qrr 'query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; tramp対策。
(recentf-mode 1)
(define-key global-map (kbd "C-c C-f") 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)
(setq wgrep-enable-key "r")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行末の空白を削除
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
(global-whitespace-mode 1)
(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'whitespace-action) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asciidoc mode
(add-to-list 'auto-mode-alist (cons "\\.asc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

;; (autoload 'doc-mode "doc-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
;; (add-to-list 'auto-mode-alist '("\\.asc$" . doc-mode))
;; (add-hook 'doc-mode-hook
;;        '(lambda ()
;;           (turn-on-auto-fill)
;;           (require 'asciidoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format mode
(let ((d (directory-files "/usr/share/emacs/site-lisp" t "clang-format-")))
  (cond (d (load (expand-file-name "clang-format" (car d))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-adjust mode
(require 'text-adjust)

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
(put 'narrow-to-region 'disabled nil)
