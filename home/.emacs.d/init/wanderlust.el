;; wanderlust

(el-get-bundle wanderlust)
;; 種々設定
(setq wl-from "Hiroyuki Ono <hiroyuki.ono.jc@renesas.com>"
      wl-organization "Renesas Electronics Corp.")

(setq elmo-message-fetch-confirm nil    ; fetchの確認をしない
      mime-edit-split-message    nil ; 大きいメッセージを送信時に分割しない
      wl-interactive-exit        t ; Wanderlust を終了する時は確認する。
      )

;; サマリバッファ
(setq wl-summary-line-format          "%-5n%T%P%1@ %Y-%M-%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
      wl-thread-insert-opened         t ; 初期設定は nil。Non-nil なら最初から thread が開かれた状態でサマリに表示されます。
      wl-summary-exit-next-move       nil ; 初期設定は t。Non-nil なら、サマリを終了するときに次のフォルダに移動します。
      wl-summary-width                nil ; 初期設定は 80。サマリの表示幅を設定された値に切り詰めます。nil なら表示幅を切り詰めません。
      wl-summary-from-width           20  ; 初期設定は 17。サマリの From 部分の表示幅です。
      wl-summary-subject-length-limit nil ; 初期設定は nil。サマリの Subject 部分の表示幅の上限です。 nil の場合、Subject 部分の長さの制限をしません。
      wl-message-window-size          '(1 . 3)
      )
(add-hook 'wl-summary-prepared-hook 'wl-summary-display-bottom) ; サマリモードに入った直後に最下部にカーソルを移動する

;; ヘッダーの表示
(setq wl-message-ignored-field-list '(".*:")
      wl-message-visible-field-list '("^To:" "^Subject:" "^From:" "^Date:" "^Cc:" "^X-Mailer:")
      wl-message-sort-field-list    '("^From:" "^To:" "^Cc:" "^Subject:" "^Date:" "^X-Mailer:"))

;; ドラフトバッファ
(setq wl-fcc                                    "+backup" ; 送ったメールは指定するディレクトリに保存しておく。
      wl-bcc                                    "hiroyuki.ono.jc@renesas.com" ; 自分をBCC
      wl-draft-reply-use-address-with-full-name nil ; 初期設定は t。 Non-nil なら返信アドレスの `To:', `Cc:' フィールドに相手のフル ネームを挿入します。nil ならアドレスだけを挿入します。
      )

;; 着信
(setq wl-biff-check-folder-list '(
                                  ".INBOX"
                                  ".1 Redmine"
                                  ".2 情報"
                                  ".3 MISC"
                                  ".4 通達"
                                  ".5 週報"
                                  ".6 組合"
                                  ".7 EDA"
                                  ".8 リスクマネジメント"
                                  ) ; モードラインに着信通知をする
      wl-biff-use-idle-timer t ; 初期設定は nil。 メールの着信をチェックするフォルダのリスト。 nil の場合は着信のチェックを行ないません。
      wl-biff-check-interval    60 ; 初期設定は 40 (単位:秒)。 この値ごとにメール着信のチェックを行ないます。
      wl-strict-diff-folders wl-biff-check-folder-list ; Use strict diff so wl-biff works with Gmail and others
      )

;; SMTP
(setq wl-smtp-posting-user "a0201089@adwin.renesas.com"
      wl-smtp-posting-server "smtp.office365.com"
      wl-smtp-connection-type 'starttls
      wl-smtp-authenticate-type "login"
      wl-smtp-posting-port 587
      wl-local-domain "renesas.com")

;; ;; IMAP
;; (setq elmo-imap4-default-user "a0201089@adwin.renesas.com"
;;       elmo-imap4-default-server "outlook.office365.com"
;;       elmo-imap4-default-stream-type 'ssl
;;       elmo-imap4-default-authenticate-type 'clear
;;       elmo-imap4-default-port '993)

;; SSL
(require 'tls)
(set-alist 'elmo-network-stream-type-alist "!opentls"
           '(opentls nil open-tls-stream))
(setq elmo-imap4-default-stream-type 'opentls)
;; (require 'tls)
;; (setq elmo-network-stream-type-alist
;;       '(("!"      ssl       ssl      open-tls-stream)
;;         ("!!"     starttls  starttls starttls-open-stream)
;;         ("!socks" socks     socks    socks-open-network-stream)))
;; (setq tls-program '("gnutls-cli -p %p %h"))

;; Display
;; text/htmlよりもtext/plainを優先表示する
(set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)

;; 初期設定では default-truncate-lines の値を使います。 Non-nil ならメッセージバッファで長い行の折り返しをしません
(setq wl-message-truncate-lines 72)
