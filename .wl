;; Wanderlust
;; ======================================================================

;; M-x wl-save
;; M-x wl-summary-pack-number
;;
;; Z	Re-read ~/.addresses
;;
;; t	thread
;; m	marked (*)
;;
;; *	mark
;; !	mark as unread
;; R	mark as read
;; $	mark as important
;; o	mark refile
;; O	mark copy
;; d	mark delete
;; i	mark as prefetch
;; u	delete all marks
;; y	save
;; U	uudecode
;; ?	search

;; all
;;     n p	move to next/previous folder/message
;;     ENTER	enter folder / view next message
;;     SPACE	enter folder / view next message
;;
;; folder mode
;;     w		new message (draft mode)
;;     m a		create folder
;;     m d		remove folder
;;     m R		rename folder
;;     C-k		cut folder
;;     C-y		paste folder
;;
;; summary mode
;;     q		back to folder mode
;;     j		move to message buffer
;;
;;     H		show all headers
;;     |		pipe message to command
;;     S		sort message
;;     v		close message buffer
;;     /		open/close thread
;;     [		open all threads
;;     ]		close all threads
;;     t s		change parent message No.
;;     s all\n		rebuild summary database
;;
;;     @		add from address to address book
;;
;;     A		reply message with citation (draft mode)
;;     f		forward message (draft mode)
;;     y		save message
;;
;;     u		unmark
;;     U		unmakr all
;;     !		mark to unread
;;     R		mark to read
;;     d		mark trashing target
;;     C-o		mark refiling target
;;     x		do trash/refile
;;     E		clear trash can
;;
;; message buffer
;;     q		back to summary mode
;;     DEL		page up
;;
;; draft mode
;;     C-c C-w	insert ~/.signature file
;;     C-c C-j	select template
;;     C-c C-c	send message
;;     C-x C-s	move message to draft folder
;; 		(to re-edit, press E in draft folder)
;;     C-c C-k	delete message
;;

(setq default-mime-charset-for-write 'utf-8)
(setq default-mime-transfer-level 8)

(setq charsets-mime-charset-alist
    (cons
	(cons (list 'unicode)	'utf-8)
	charsets-mime-charset-alist))
(setq charsets-mime-charset-alist
    (cons
	(cons (list 'ascii)	'us-ascii)
	charsets-mime-charset-alist))

(eval-after-load
    "mime-edit"
    '(let ((text (assoc "text" mime-content-types)))
	(set-alist 'text "plain"
	    '(("charset" "" "UTF-8" "ISO-2022-JP" "US-ASCII")))
	(set-alist 'mime-content-types "text" (cdr text))))

;; クォートされた文字列もデコードする
(setq mime-header-lexical-analyzer
      '(
        ;; eword-analyze-quoted-string
        eword-analyze-domain-literal
        eword-analyze-comment
        eword-analyze-spaces
        eword-analyze-special
        eword-analyze-encoded-word
        eword-analyze-atom))

;; NEC special chars
;; NEC extended chars from IBM extended chars
;; IBM extendec chars
;(add-hook 'wl-message-redisplay-hook 'izonmoji-mode-on)

;; ======================================================================

;; Servers
(setq
    wl-smtp-posting-server "127.0.0.1"
    wl-smtp-posting-port 25
)

;; Workaround for http://lists.airs.net/wl/archive/200602/msg00027.html
;(setq smtp-end-of-line "\n")

;(setq wl-nntp-posting-server "news.example.jp")
;(setq elmo-default-imap4-server "imap4.example.jp")
;(setq elmo-default-nntp-server "news.example.jp")

(when (coding-system-p 'cp50220)
    (add-to-list 'mime-charset-coding-system-alist '(iso-2022-jp . cp50220))
)

; Identifier
(setq wl-from "SATOH Fumiyasu <fumiyas@osstech.jp>")
(setq wl-organization "OSS Technology Corp. / Samba-JP / LDAP-JP / Apache-JP")
;(setq wl-organization "OSS Technology Corp. / Samba-JP / Namazu Developer")
(setq wl-local-domain "sugar.osstech.co.jp")
;(setq wl-local-domain "sugar.lan.sfo.jp")
(setq wl-user-mail-address-list
    '(
	"fumiyas@samba.gr.jp"
	"fumiyas@osstech.jp"
	"fumiyas@osstech.co.jp"
	"fumiyas@miraclelinux.com"
	"fumiyas@samba.gr.jp"
	"fumiya@samba.gr.jp"
	"satoh.fumiyasu@gmail.com"
    )
)

; wl icon directory
;(setq wl-icon-dir "~/.wlrc/icon")

; Online mode by default
(setq wl-plugged t)

;; Invoke imget on wl: M-i
;; http://www.lab3.kuis.kyoto-u.ac.jp/%7Etsumura/emacs/wl.html
(defun TT:wl-inc-mail ()
    (interactive)
    (message "Incing ... ")
    (call-process "ig" nil nil nil)
    ;(call-process "imget" nil nil nil)
    (if (and (boundp 'wl-summary-buffer-folder-name)
             (eq wl-summary-buffer-folder-name wl-default-folder)
	)
        (wl-summary-sync-force-update)
      (wl-summary-goto-folder-subr wl-default-folder 'force-update nil nil)
    )
)
(add-hook 'wl-summary-mode-hook
    (function
	(lambda ()
	    (define-key wl-summary-mode-map "\M-i" 'TT:wl-inc-mail)
	)
    )
)

;; Folder mode
;; ======================================================================

(setq wl-folder-notify-delete "sync")

(setq wl-folder-sync-range-alist
    '(
	("^&.*$" . "all")
	("^\\+draft$\\|^\\+queue$" . "all")
    )
)

;; Summary mode
;; ======================================================================

(setq wl-summary-width nil)

;; Stay folder view in summary mode
;(setq wl-stay-folder-window t)

;; Default folder spec when correcting folder name
(setq wl-default-spec "+")

;; Show nickname defined in ~/.addresses
(setq wl-use-petname t)

;; If `From' is my address, show 'To' header instead.
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

;; Prevent from summarizing subject in message from mailing list
(setq wl-summary-subject-function 'my-wl-summary-subject-func-ml)
(defun my-wl-summary-subject-func-ml (subject-string) subject-string)


;; View thread as be opened
(setq wl-thread-insert-opened t)
;; スレッド表示のインデントを無制限にする。
;(setq wl-summary-indent-length-limit nil)
;(setq wl-summary-width nil)
;; サブジェクトが変わったらスレッドを切って表示
(setq wl-summary-divide-thread-when-subject-changed t)
;; ...
;(setq wl-summary-search-parent-by-subject-regexp nil)

;; サマリ内の移動で未読メッセージがないと次のフォルダに移動しない
(setq wl-auto-select-next nil)
;; メッセージに既読/未読に関係なく前後のメッセージに移動
(setq wl-summary-move-order nil)

;; Summary モードで n,p で読み進めて最後か最初までいったときフォルダを抜けない
(add-hook 'wl-summary-mode-hook
  '(lambda ()
    (setq wl-summary-buffer-prev-folder-function 'ignore wl-summary-buffer-next-folder-function 'ignore
    )
  )
)

;; Marking
;; ----------------------------------------------------------------------

(setq wl-summary-auto-refile-skip-marks
    '(
	"N"
	"D"
	"U"
	"!"
	"$"
    )
)
(setq wl-summary-skip-mark-list
    '(
	"."	; dummy
    )
)

; Refile
; ----------------------------------------------------------------------

(setq elmo-msgdb-extra-fields
    '(
	"return-path"
	"content-type"
    )
)

(setq wl-refile-rule-alist
    '(
        ("Subject"
	    ("^VMware Newsletter " . "+ml/member/vmware")

	    ("^\\[samba-jp:" . "+ml/samba/samba-jp")
	    ("^\\[sugj-free:" . "+ml/samba/sugj-free")
	    ("^\\[sugj-staff:" . "+ml/samba/sugj-staff")
	    ("^\\[sugj-admin:" . "+ml/samba/sugj-admin")
	    ("^\\[sugj-tech:" . "+ml/samba/sugj-tech")
	    ("^\\[sugj-web:" . "+ml/samba/sugj-web")

	    ("^\\[debian-users:" . "+ml/linux/debian-users")
	    ("^\\[debian-devel:" . "+ml/linux/debian-devel")

	    ("^\\[LDAP-Staff:" . "+ml/net/ldap-staff")
	    ("^\\[LDAP-Users:" . "+ml/net/ldap-users")
	    ("^\\[mailman-users-jp " . "+ml/net/mailman-users-jp")
	    ("^\\[DNSOPS " . "+ml/net/dnsops")
	    ("^\\[rt100i-users" . "+ml/net/rt100i-users")
	    ("\\[hyperestraier-" . "+ml/misc/hyperestraier")
	    ("^\\[jsosug:" . "+ml/linux/jsosug")

	    ("^\\[installer " . "+ml/misc/installer")
	)
        (("To" "Cc")
	    ("dns@list.cr.yp.to" . "+ml/net/djb-dns")

	    ("-admin-@samba.gr.jp" . "+ml/samba/sugj-bounce")
	    ("-admin-.+=[a-z0-9.-]+@samba.gr.jp" . "+ml/samba/sugj-bounce")
	)
        ("Return-Path"
	    ("<[a-z]+-support-bounces\\+.*@osstech.co.jp" . "+work/osstech/support")
	    ("<ost-consult-bounces\\+.*@osstech.co.jp" . "+work/osstech/consult")
	    ("<ost-sys-bounces\\+.*@osstech.co.jp" . "+work/osstech/sys")
	    ("<ost-syslog-bounces\\+.*@osstech.co.jp" . "+work/osstech/sys")
	    ("<ost-sys-.*-bounces\\+.*@osstech.co.jp" . "+work/osstech/sys")
	    ("<ost-syslog-bounces\\+.*@osstech.co.jp" . "+work/osstech/sys")
	    ("<ost-tech-bounces\\+.*@osstech.co.jp" . "+work/osstech/tech")

	    ("samba-technical-bounces" . "+ml/samba/samba-technical")
	    ("samba-technical-bounces" . "+ml/samba/samba-technical")
	    ("openldap-technical-bounces" . "+ml/net/openldap-technical")
	    ("openldap-devel-bounces" . "+ml/net/openldap-technical")

	    ("solaris2u-bounces@" . "+ml/os/solaris2u")
	    ("users-bounces@opensolaris" . "+ml/os/solaris-ug")
	    ("begin-bounces@ml.begi.net" . "+ml/linux/beginet-begin")
	    ("freetalk-bounces@ml.begi.net" . "+ml/linux/beginet-freetalk")

	    ("dovecot-bounces@" . "+ml/net/dovecot")
	    ("postfix-jp-list-bounces@" . "+ml/net/postfix-jp")
	    ("milter-manager-users-ja-bounces@" . "+ml/net/milter-manager-users-ja")
	    ("spamassassin-jp-bounces\\+.*@" . "+ml/net/spamassassin-jp")
	    ("clamav-jp-users-bounces@" . "+ml/misc/clamav-jp")
	    ("ml-admin@mysql.gr.jp" . "+ml/db/mysql-jp")

	    ("apache-core-bounces\\+.*@apache.jp" . "+ml/web/apache-core")
	    ("apache-users-bounces.*@apache.jp" . "+ml/web/apache-users")
	    ("announce-bounces\\+.*@apache.jp" . "+ml/web/apache-announce")

	    ("groonga-dev-bounces@" . "+ml/misc/groonga-dev")
	    ("namazu-devel-..-bounces@namazu.org" . "+ml/misc/namazu-devel")
	    ("namazu-cvs-bounces@namazu.org" . "+ml/misc/namazu-devel")
	    ("vaj-admin-bounces@namazu.org" . "+ml/misc/namazu-admin")
	    ("project-ja-bounces@namazu.org" . "+ml/misc/namazu-project")

	    ("ruby-list-bounces@" . "+ml/develop/ruby-list")
	    ("ruby-dev-bounces@" . "+ml/develop/ruby-dev")
	    ("rails=return=" . "+ml/develop/rails")
	    ("skk-return-" . "+ml/misc/skk")
	    ("nakajima.yasushi-pdfj-return-" . "+ml/misc/pdfj")
	    ("edict-return-" . "+ml/misc/edict")
	    ("gunma-ct-bounces@" . "+ml/friend/gunma-ct")
	)
    )
)

;; 添付ファイルがある場合は「@」を表示
;(setq wl-summary-line-format "%n%T%P%1@%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %#%~%s")
(setq wl-summary-line-format "%n%T%P%1@%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %~%s")
(setq wl-summary-line-format-spec-alist
      (append wl-summary-line-format-spec-alist
              '((?@ (wl-summary-line-attached)))))

; Message(?) mode
; ======================================================================

(setq wl-message-visible-field-list '())

(setq wl-message-ignored-field-list '(
    "^Sender:"
    "^Return-Path:"
    "^Received:"
    "^Received-SPF:"
    "^X-Received:"
    "^X-post-Received:"
    "^Old-Return-Path:"
    "^Delivered-To:"
    "^References:"
    "^In-Reply-To:"
    "^Errors-To:"
    "^Message-ID:"
    "^Resent-Message-ID:"
    "^MIME-Version:"
    "^Content-Disposition:"
    "^Content-Transfer-Encoding:"
    "^Content-class:"
    "^Lines:"
    "^Posted:"
    "^X-Authenticated-"
    "^Precedence:"
    "^Priority:"
    "^X-Priority:"
    "^User-Agent:"
    "^X-Face:"
    "^Face:"
    "^DKIM-Signature:"
    "^DomainKeys-Signature:"
    "^X-Greylist:"
    "^X-Mailer:"
    "^X-ML-"
    "^X-MLServer:"
    "^X-Mail-Count"
    "^X-Sequence"
    "^List-"
    "^Mailing-"
    "^X-Mailing-List"
    "^X-list"
    "^X-archive-position:"
    "^X-ecartis-version:"
    "^X-Saved-"
    "^X-Disclaimer:"
    "^X-Mailman-Version:"
    "^X-BeenThere:"
    "^X-Accept-Language:"
    "^Status:"
    "^X-UIDL:"
    "^X-Flags:"
    "^X-Scanned-By:"
    "^X-Virus-"
    "^X-Quarantine-ID:"
    "^X-IronPort-"
    "^X-IRONPORT:"
    "^X-MS-Has-Attatch:"
    "^X-MS-TNEF-Correlator:"
    "^X-MimeOLE:"
    ;"^Received-SPF:"
    "^Authentication-Results:"
    "^DomainKey-Signature:"
    "^X-Spam-"
    "^X-ESAFE-"
    "^X-MSW-SpamLogic:"
    "^Thread-:"
    "^Resent-Date:"
    "^X-OriginalArrivalTime:"
    "^X-No-Archive:"
    "^X-Loop:"
    "^X-ExtLoop1:"
    "^X-Original-To:"
    "^X-Original-Received:"
    "^X-Debian:"
    "^X-vs:"
    "^X-Git-"
))

(setq wl-message-sort-field-list '(
    "^To"
    "^Cc"
    "^Subject"
    "^From"
    "^Reply-To"
    "^Date"
    "^Message-ID"
))

;(setq wl-message-ignored-field-list nil)
;(setq mime-view-ignored-field-list '(
;    ".*Received:" ".*Path:" ".*Id:" "^References:"
;    "^Replied:" "^Errors-To:"
;    "^Lines:" "^Sender:" ".*Host:" "^Xref:"
;    "^Content-Type:" "^Content-Transfer-Encoding:"
;    "^Precedence:"
;    "^Status:" "^X-VM-.*:"
;    "^X-Info:" "^X-PGP" "^X-Face-Version:"
;    "^X-UIDL:" "^X-Dispatcher:"
;    "^MIME-Version:" "^X-ML" "^Message-I.:"
;    "^Delivered-To:" "^Mailing-List:"
;    "^ML-Name:" ; "^mail-count:"
;    "^Reply-To:" "^In-Reply-To:" "Date:"
;    "^X-Loop" "^X-List-Help:"
;    "^X-Trace:" "^X-Complaints-To:"
;))

;; Draft mode
;; ======================================================================

(setq wl-auto-save-drafts-interval 15)

; draft時にskk起動
;(add-hook 'wl-mail-setup-hook
;(function
;(lambda ()
;(wl-draft-config-exec)
;(skk-mode)
;)))

;; Re: Re[2]: -> Re:
(setq wl-subject-re-prefix-regexp
  "^[ \t]*\\([Rr][Ee]\\(\\[[0-9]+\\]\\)?[:>][ \t]*\\)*[ \t]*")

; File Carbon Copy
(setq wl-fcc "+sent")
(setq wl-fcc-force-as-read t)  ; IMAP4 only
; Auto Blind Carbon Copy
;(setq mail-self-blind t)
; 
(setq wl-draft-always-delete-myself nil)

;;
;(signature-insert-at-eof t)
;; ドラフトを新しいフレームで書く
(setq wl-draft-use-frame t)

; Copy `Reply-To' to `To' header when reply message
;(setq wl-draft-reply-without-argument-list
;    (append '(("Reply-To" . (("Reply-To") nil nil)))
;               wl-draft-reply-without-argument-list
;    )
;)
(setq wl-draft-reply-without-argument-list
    '(
	("Reply-To" . (("Reply-To") nil nil))
	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
	("Followup-To" . (nil nil ("Followup-To")))
	("From" . (("From") ("To" "Cc") ("Newsgroups")))
    )
)

; Message-ID field
(setq wl-insert-message-id t)
(setq wl-message-id-domain "sugar.osstech.co.jp")
;(setq wl-message-id-domain "sugar.lan.sfo.jp")
;(setq wl-message-id-domain (concat (system-name) ".lan.sfo.jp"))

(add-hook 'wl-mail-setup-hook
    '(lambda ()
	;; C-x C-s 時のコード指定を自動化
	(set-buffer-file-coding-system 'iso-2022-jp)
	;; 再編集時に wl-draft-config-alist を適用しない
	(unless wl-draft-reedit
	    (wl-draft-config-exec wl-draft-config-alist)
	)
;	(if wl-draft-reply
;	    (wl-draft-config-exec wl-draft-reply-config-alist)
;	    (wl-draft-config-exec wl-draft-new-config-alist)
;	)
    )
)

;; User-Agent 名を簡素化
(setq wl-generate-mailer-string-function 'wl-generate-user-agent-string-1)

;; Confirm before sending
(setq wl-interactive-send t)

;; Remove '*** SPAM ***' from Subject: on replying
(add-hook 'wl-draft-reply-hook
	  (function
	  (lambda ()
	    (save-excursion
	      (beginning-of-buffer)
	      (re-search-forward "^Subject: " (point-max) t)
	      (while (re-search-forward
		      "\\*\\*\\*\\(SPAM\\|UNCHECKED\\)\\*\\*\\* *"
		      (save-excursion (end-of-line) (point)) t)
		(replace-match ""))
	      ))))

;; envelope-from を From: ヘッダーのアドレスに設定
(add-hook 'wl-draft-send-hook
    (lambda ()
	(set (make-local-variable 'wl-from) (std11-fetch-field "From"))
    )
)
(setq wl-draft-config-alist
    '(
;	("regexp"
;	    (wl-smtp-posting-server . "smtp.example.com")
;	    (wl-draft-send-mail-function . 'wl-draft-send-mail-with-pop-before-smtp)
;	    (wl-pop-before-smtp-user . "xxxxxxxxxxx")
;	    (wl-pop-before-smtp-server . "xxxxxxxxxxx")
;	)
	(reply
	    "Delivered-To: ost-"
	    (template . "osstech")
	)
	(reply
	    "Delivered-To: fumiyas@osstech\\.jp"
	    (template . "osstech-jp")
	)
;	("\\(To\\|B?cc\\):.*\\(\n[ \t]+.*\\)*@\\(miraclelinux\\|.*\\.\\(sra\\|hitachi.*\\)\\)\\."
;	    (template . "miracle")
;	)
	("\\(To\\|From\\):.*\\(\n[ \t]+.*\\)*satoh.fumiyasu@gmail\\.com"
	    (template . "gmail")
	)
	("Delivered-To: fumiyas?@samba\\.gr\\.jp"
	    (template . "samba")
	)
;	("Delivered-To: fumiyas?@net-thrust\\.com"
;	    (template . "thrust")
;	)
;	("\\(To\\|B?Cc\\):.*\\(\n[ \t]+.*\\)*@tezuka\\."
;	    (template . "thrust")
;	)
;	("To: west-hp@ntt-ad\\.co\\.jp"
;	    ("Cc" . "nttw-proj@net-thrust.com")
;	)
;	("\\(To\\|B?Cc\\):.*\\(\n[ \t]+.*\\)*@\\(fujielectric\\|sts-inc\\|jet-t\\)\\."
;	    (template . "jet")
;	)
    )
)

;; 送信 (C-c C-c) 時に wl-draft-config-alist を適用しない
(remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

;; Template
;; ----------------------------------------------------------------------

(setq wl-template-alist
    '(
	("osstech-int"
	    ("From" . "SATOH Fumiyasu <fumiyas@osstech.co.jp>")
	    (top . "さとうふみやす です。\n\n")
;	    (body. "body")
	)
	("osstech"
	    ("From" . "SATOH Fumiyasu <fumiyas@osstech.co.jp>")
	    (top . "様\n\nOSSTech さとうふみやす です。\nお世話になっております。\n\n")
;	    (body. "body")
	)
	("osstech2"
	    ("From" . "SATOH Fumiyasu <fumiyas@osstech.co.jp>")
	    (top . "様\n\nさとうふみやす です。\nさとうふみやす @ OSSTech です。\nOSSTech さとうふみやす です。\nお世話になっております。\n\n")
;	    (body. "body")
	)
	("osstech-jp"
	    ("From" . "SATOH Fumiyasu <fumiyas@osstech.jp>")
	    (top . "さとうふみやす @ OSSTech です。\n\n")
	)
	("sfo.jp"
	    ("From" . "SATOH Fumiyasu <fumiyas@sfo.jp>")
	    (top . "さとうふみやす です。\n\n")
	)
	("gmail"
	    ("From" . "SATOH Fumiyasu <satoh.fumiyasu@gmail.com>")
	    (top . "ふみやす です。\n\n")
	)
;	("report"
;	    ("From" . wl-from)
;	    ("To" . "Kondara-users@kondara.org")
;	    ("Subject" . "報告")
;	    (body-file . "~/work/report.txt")
;	)
    )
)

; Expire and Archive
; ======================================================================

(setq wl-expire-use-log t)

(setq wl-expire-alist
    '(
	("^\\+trash$" (date 60) remove)
	("^\\+dust$" (date 60) remove)
	("^\\+work/ntt-west-admin$" (date 180) remove)
;	("^\\+ml/samba/sugj-" (date 365) remove)
;	("^\\+ml/samba/samba-" (date 365) remove)
	("^\\+ml/linux" (date 365) remove)
	("^\\+ml/web/new-httpd$" (date 365) remove)
	("^\\+ml/web/php-users$" (date 365) remove)
	("^\\+ml/web/cgi$" (date 365) remove)
	("^\\+ml/net/djb-" (date 365) remove)
;	("^\\+ml/net/rt100i" (date 365) remove)
	("^\\+ml/misc/security-memo" (date 365) remove)
;	("^\\+ml/misc/namazu-devel" (date 365) remove)
	("^\\+ml/misc/openoffice" (date 365) remove)
	("^\\+.*" (date 365) remove)
    )
)

; Misc
; ======================================================================

; Elmo
; ----------------------------------------------------------------------

(setq elmo-msgdb-default-type 'standard
      elmo-msgdb-convert-type 'auto
)

; Face (Coloring)
; ----------------------------------------------------------------------

;; highlightの設定 (明るい背景色の場合)

(setq wl-highlight-message-header-alist
      '(("Subject[ \t]*:" . wl-highlight-message-subject-header-contents)
        ("From[ \t]*:" . wl-highlight-message-from-header-contents)
        ("\\(.*Reply-To\\|.*Followup-To\\)[ \t]*:" . wl-highlight-message-replyto-header-contents)
        ("\\(.*To\\|Cc\\|Newsgroups\\)[ \t]*:" . wl-highlight-message-to-header-contents)
        ("\\(User-Agent\\|X-Mailer\\|X-Newsreader\\)[ \t]*:" .
         wl-highlight-message-important-header-contents)
        ))
;; 引用レベルで色分けしない
;(setq wl-highlight-citation-face-list
;      '(wl-highlight-message-cited-text-1))

;; メッセージヘッダ
(copy-face 'bold 'wl-highlight-message-headers)
(set-face-foreground 'wl-highlight-message-headers "magenta3")
(copy-face 'default 'wl-highlight-message-header-contents)
(set-face-foreground 'wl-highlight-message-header-contents "brown")
(copy-face 'bold 'wl-highlight-message-subject-header-contents)
(set-face-foreground 'wl-highlight-message-subject-header-contents "blue2")
(copy-face 'bold 'wl-highlight-message-from-header-contents)
(set-face-foreground 'wl-highlight-message-from-header-contents "red3")
(copy-face 'bold 'wl-highlight-message-to-header-contents)
(set-face-foreground 'wl-highlight-message-to-header-contents "DarkOrange2")
(copy-face 'default 'wl-highlight-message-replyto-header-contents)
(set-face-foreground 'wl-highlight-message-replyto-header-contents "DarkOrange2")
(copy-face 'bold 'wl-highlight-message-important-header-contents)
(set-face-foreground 'wl-highlight-message-important-header-contents "purple")
(copy-face 'default 'wl-highlight-message-unimportant-header-contents)
(set-face-foreground 'wl-highlight-message-unimportant-header-contents "RoyalBlue")
;; 引用
(set-face-foreground 'wl-highlight-message-citation-header "DarkGreen")
(set-face-foreground 'wl-highlight-message-cited-text-1 "forest green")
(set-face-foreground 'wl-highlight-message-cited-text-2 "SaddleBrown")
(set-face-foreground 'wl-highlight-message-cited-text-3  "orchid3")
(set-face-foreground 'wl-highlight-message-cited-text-4  "purple1")
(set-face-foreground 'wl-highlight-message-cited-text-5  "MediumPurple1")
(set-face-foreground 'wl-highlight-message-cited-text-6  "PaleVioletRed")
(set-face-foreground 'wl-highlight-message-cited-text-7  "LightPink")
(set-face-foreground 'wl-highlight-message-cited-text-8  "salmon")
(set-face-foreground 'wl-highlight-message-cited-text-9  "SandyBrown")
(set-face-foreground 'wl-highlight-message-cited-text-10 "wheat")

;; サマリ
;(set-face-foreground 'wl-highlight-summary-important-face "purple")
(set-face-foreground 'wl-highlight-summary-new-face "tomato")
(set-face-foreground 'wl-highlight-summary-displaying-face "blue2")
(set-face-foreground 'wl-highlight-summary-unread-face "RoyalBlue")
(set-face-foreground 'wl-highlight-summary-deleted-face "gray")
(set-face-foreground 'wl-highlight-summary-refiled-face "blue")
(set-face-foreground 'wl-highlight-summary-temp-face "salmon")
;; (スレッド)
(set-face-foreground 'wl-highlight-summary-thread-top-face "green4")
(set-face-foreground 'wl-highlight-summary-normal-face "SeaGreen")

;; フォルダ
(set-face-foreground 'wl-highlight-folder-unknown-face "RoyalBlue")
(set-face-foreground 'wl-highlight-folder-killed-face "gray50")
(set-face-foreground 'wl-highlight-folder-unread-face "brown")
(set-face-foreground 'wl-highlight-folder-zero-face "blue4")
(set-face-foreground 'wl-highlight-folder-few-face "orange")
(set-face-foreground 'wl-highlight-folder-many-face "HotPink1")
;; グループ
(set-face-foreground 'wl-highlight-folder-opened-face "forest green")
(set-face-foreground 'wl-highlight-folder-closed-face "DarkOliveGreen4")

;; デモ
(set-face-foreground 'wl-highlight-demo-face "blue2")

;; ======================================================================

;; 初期フレームの設定
(setq initial-frame-alist
    (append '(
	(width	. 90)	; フレーム幅(文字数)
	(height	. 40)	; フレーム高(文字数)
    ) initial-frame-alist)
)

;; 新規フレームの設定
;(setq default-frame-alist
;    (append '(
;	(width	. 90)	; フレーム幅(文字数)
;	(height	. 40)	; フレーム高(文字数)
;    ) default-frame-alist)
;)

