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
             '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Initialize
(package-initialize)
(setq installing-package-list '(
         auto-complete
         markdown-mode
         google-c-style
         yaml-mode
         open-junk-file
         dired-filter ;; diredでディレクトリ・特定の拡張子・ファイル名の正規表現にマッチしたもののみを表示 http://rubikitch.com/2015/04/07/dired-filter/
         number ;; カーソル位置の数字に対して簡単な計算をしたり形式を整えたり http://rubikitch.com/2015/06/21/number/
         ddskk
         ))
(when (executable-find "git")
  (setq installing-package-list (append '(magit magit-gitflow gist git-gutter) installing-package-list)))
(when (executable-find "ag")
  (setq installing-package-list (append '(ag wgrep-ag) installing-package-list)))
(when (executable-find "asciidoc")
  (setq installing-package-list (append '(adoc-mode) installing-package-list)))

(require 'cl)
(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start for emacs-client
(when
    (and
     (executable-find "emacsclient")
     (=
      (with-temp-buffer
        (call-process "/bin/bash" nil (current-buffer) nil "-c" "cat /proc/self/cgroup | grep cpu:/docker")
        )
      0))
  (require 'server)
  (unless (server-running-p)
    (server-start))
) ;; end of (executable-find "emacsclient")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; auto-complete
(require 'auto-complete-config)
(ac-config-default)

(when (executable-find "git")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Magit
  (require 'magit)
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  ;; (setq magit-diff-options '("-b"))
  (define-key global-map (kbd "C-h m") 'magit-status)
  (define-key global-map (kbd "C-x m") 'magit-status)
  (eval-after-load 'magit
    '(progn
       (define-key magit-mode-map (kbd "C-h") 'delete-backward-char)
       ;; (set-face-background 'magit-diff-add "#222222")
       ;; (set-face-background 'magit-diff-del "#222222")
       ;; (set-face-foreground 'magit-diff-file-header "#0000ff")
       ;; (set-face-background 'magit-diff-file-header "#888888")
       ;; (set-face-foreground 'magit-log-head-label-bisect-skip "black")
       ;; (set-face-foreground 'magit-log-head-label-tags "black")
       ;; (set-face-foreground 'magit-log-reflog-label-commit "black")
       ;; (set-face-foreground 'magit-tag "black")
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
) ;; end of (executable-find "git")

(when (executable-find "ag")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ag & wgrep
  (setq default-process-coding-system 'utf-8-unix)  ; ag 検索結果のエンコード指定
  (require 'ag)
  (setq ag-highlight-search t)  ; 検索キーワードをハイライト
  (setq ag-reuse-buffers t)     ; 検索用バッファを使い回す (検索ごとに新バッファを作らない)
                                        ; wgrep
  (add-hook 'ag-mode-hook '(lambda ()
                             (require 'wgrep-ag)
                             (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
                             (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
                             (wgrep-ag-setup)))

  ) ;; end of (executable-find "ag")

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
;; ispell mode
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asciidoc mode
(when (executable-find "asciidoc")
  (add-to-list 'auto-mode-alist (cons "\\.asc\\'" 'adoc-mode))
  (add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))
  (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t))))
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
;; google-c-style
(cond ((require 'google-c-style nil t)
       (defun my-c-c++-mode-init ()
         (google-set-c-style)
         ;; (google-make-newline-indent)
         )
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-adjust mode
(require 'text-adjust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; number
(require 'number)
;; (global-set-key (kbd "C-c M-+") 'number/add)
;; (global-set-key (kbd "C-c M--") 'number/sub)
;; (global-set-key (kbd "C-c M-*") 'number/multiply)
;; (global-set-key (kbd "C-c M-/") 'number/divide)
;; (global-set-key (kbd "C-c M-0") 'number/pad)
;; (global-set-key (kbd "C-c M-=") 'number/eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; japanese inputmehod
(cond ((require 'mozc nil t)
       (setq default-input-method "japanese-mozc")
       (define-key global-map (kbd "C-\\") 'toggle-input-method))
      ((require 'skk nil t)
       (define-key global-map (kbd "C-\\") 'toggle-input-method)
       (define-key global-map (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
       (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
       (add-hook 'skk-load-hook
                 (lambda ()
                   (require 'skk-study)                               ;;変換学習機能の追加
                   (require 'context-skk)
                   ))
       (add-to-list 'context-skk-programming-mode 'c++-mode)
       ))

(message "Loaded")
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-skk ddskk number dired-filter open-junk-file yaml-mode google-c-style markdown-mode git-gutter gist magit-gitflow magit wgrep-ag ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
