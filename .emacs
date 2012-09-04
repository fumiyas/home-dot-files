;; M-% とか矩形カットアンドペースト(C-x r k、C-x r y)
;; M-x describe-key
;;
;; C-@ <move> C-w	cut
;; C-@ <move> M-w	yank
;;
;; M-.			Tag jump
;; M-*			Tag reverse jump
;; M-x tags-search	Tag search
;; M-,			Tag search next
;;

;(when (string= (getenv "TERM") "jfbterm")
;    (set-terminal-coding-system 'iso-2022-7bit))

;; <http://www.jsdlab.co.jp/~kei/xyzzy/manual/basic/basic.html>

;; ======================================================================

;; Helper functions to initialize the Emacs safely
;; <http://www.sodan.org/~knagano/emacs/dotemacs.html>
;; ----------------------------------------------------------------------

(defun load-safe (libname)
    "安全な load。読み込みに失敗してもそこで止まらない。"
    ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
    (let ((load-status (load libname t)))
	(or load-status
	    (message (format "[load-safe] failed %s" libname))
	)
	load-status
    )
)

(defmacro exec-if-bound (plist)
    "関数が存在する時だけ実行する。(car の fboundp を調べるだけ)"
    `(if (fboundp (car ',plist))
	,plist
    )
)

(defun autoload-if-found (function file &optional docstring interactive type)
    "set autoload iff. FILE has found."
    (if (locate-library file)
	(autoload function file docstring interactive type)
	nil
    )
)

;; ----------------------------------------------------------------------

(when (and (featurep 'toolbar) (fboundp 'set-specifier))
  (defun tool-bar-mode (&optional arg)
    (interactive)
    (set-specifier default-toolbar-visible-p
	   (if (null arg)
	       (not (specifier-instance default-toolbar-visible-p))
	     (> (prefix-numeric-value arg) 0)))))

;; File and Directory
;; ======================================================================

(setq load-path
    (append
	(list
	    (expand-file-name "~/lib/elisp")
	)
	load-path
    )
)

(if (featurep 'xemacs)
    (setq load-path
	(append
	    (list
		(expand-file-name "~/lib/elisp/xemacs")
	    )
	    load-path
	)
    )
)

(setq temporary-file-directory (expand-file-name "~/var/tmp"))

;; hoge.txt~ みたいなバックアップファイルを作らないようにする
;(setq backup-inhibited t)

;; Load and Save
;; ----------------------------------------------------------------------

(setq auto-save-default t)
(setq auto-save-interval 50)

;; Enable `Open Recent' menu
(exec-if-bound (recentf-mode))

;; Terminal
;; ======================================================================

;(if (not (featurep 'xemacs))
    (when (locate-library "jisx0213")
	(require 'jisx0213)
    )
;)

;; Use Mule-UCS
(if (featurep 'xemacs)
    (require 'un-define)
)
;(un-define-debian)
;(un-define-debian-jisx0213)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(if (not window-system)
    (set-terminal-coding-system 'utf-8)
)

;; ---- language-env DON'T MODIFY THIS LINE!
;; 日本語 info が文字化けしないように
(auto-compression-mode t)
;; xemacs の shell-mode で 日本語 EUC が使えるようにする
;(add-hook 'shell-mode-hook (function
;   (lambda () (set-buffer-process-coding-system 'euc-japan 'euc-japan))))
;; 日本語 grep
;(if (file-exists-p "/usr/bin/jgrep")
;    (setq grep-command "jgrep -n -e ")
;)

; Don't display startup message
(setq inhibit-startup-message t)

(when window-system
    (tool-bar-mode -1)
    (when (not (featurep 'xemacs))
	(auto-image-file-mode t)
	(mouse-wheel-mode t)
    )
)

(setq visible-bell nil)
(setq truncate-partial-width-windows t)

(setq mouse-yank-at-point t)

(setq-default line-spacing 0)

(load-safe "bitmap-ci")

(when (locate-library "izonmoji-mode")
  (autoload 'izonmoji-mode "izonmoji-mode" nil t)
  (autoload 'izonmoji-mode-on "izonmoji-mode" nil t)
  (add-hook 'w3m-mode-hook 'izonmoji-mode-on)
  (add-hook 'mew-message-mode-hook 'izonmoji-mode-on)
  (add-hook 'wl-message-redisplay-hook 'izonmoji-mode-on))

;; Edit
;; ======================================================================

(setq next-line-add-newlines nil)
(setq track-eol t)
(setq-default indent-tabs-mode nil)
(load-library "paren")

;; 折り返し桁数の初期値
(setq-default fill-column 999)

;; Listing kill-ring
(when (autoload-if-found 'kill-summary "kill-summary" nil t)
    (global-set-key "\M-y" 'kill-summary)
)

;; Save kill-ring and file-name-history when exit
(when (load-safe "session")
    (add-hook 'after-init-hook 'session-initialize)
)

;; word completion by words in all buffers
(load-safe "dabbrev-ja") ;; M-/
(load-safe "dabbrev-highlight")
;(global-set-key "\C-j" 'dabbrev-expand)

;; Search
;; ======================================================================

;; i-search for Japanese
(load-safe "migemo")
(setq migemo-coding-system 'utf-8)
;(setq migemo-command "/usr/bin/cmigemo")

;; i-search for filename in mini-buffer
(load-safe "minibuf-isearch")

;; i-search のハイライトの反応をよくする
(setq isearch-lazy-highlight-initial-delay 0)

;; Not work...
;(completion-ignore-case t)

;(load-safe "zlc")

;; Key binding
;; ======================================================================

;; Delete chars on cursor by Delete key
(global-set-key [delete] 'delete-char)
;; Backspace by C-h
(global-set-key "\C-h" 'backward-delete-char)

;; Imput Method
;; ----------------------------------------------------------------------

;(setq load-path (delete "/usr/share/xemacs21/site-lisp/ddskk" load-path))
;(skk-mode)
;(setq default-input-method "japanese-skk")

(if (not (file-directory-p "~/.skk"))
      (make-directory "~/.skk"))
(setq skk-init-file "~/.skk/init.el"
    skk-custom-file "~/.skk/custom.el"
    skk-emacs-id-file "~/.skk/emacs-id"
    skk-record-file "~/.skk/record"
    skk-jisyo "~/.skk/jisyo"
    skk-backup-jisyo "~/.skk/jisyo.bak")
(set-input-method "japanese-skk")
;; commented out at 2005-12-14...
;(toggle-input-method nil)

;(setq egg-mode-preference nil)

;; Highlighting
;; ----------------------------------------------------------------------

(setq transient-mark-mode t)
(setq search-highlight t)
(setq query-replace-highlight t)

;; 無駄な空行に気付きやすくする
(setq-default indicate-empty-lines t)

;; Coloring
;; ----------------------------------------------------------------------

(require 'font-lock)
(if (not (featurep 'xemacs))
    (global-font-lock-mode t)
)
(add-hook 'font-lock-mode-hook
    '(lambda ()
	(make-face 'comment-face)
	(set-face-foreground 'comment-face "bisque1")
	(setq font-lock-comment-face 'comment-face)
    )
)

;(when (eq (console-type) 'x)
(when (eq window-system 'x)
    (set-face-background 'default "lightyellow1")
    (set-face-foreground 'default "black")
    (set-cursor-color "red")
)

(defface ws-face-r-1 '((t (:background "cyan"))) nil)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (defvar ws-face-r-1 'ws-face-r-1)
  (font-lock-add-keywords
   major-mode '(("[ 　\t\r]+$" 0 ws-face-r-1 append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; Scroll
;; ----------------------------------------------------------------------

(setq scroll-step 1)
;; 画面から出たとき一行だけスクロールさせる
(setq scroll-conservatively 1)

(exec-if-bound (set-scroll-bar-mode 'right))

;; Mode line
;; ----------------------------------------------------------------------

;(setcar (cdr (assq 'encoded-kbd-mode minor-mode-alist)) "")
(line-number-mode t)
(column-number-mode t)

;; Date & Time
;(setq display-time-24hr-format t)
;(setq display-time-day-and-date t)
;(setq display-time-string-forms
;    '(month "/" day "(" dayname ") " 24-hours ":" minutes)
;)
;(display-time)

;; Web Browser
;; ----------------------------------------------------------------------

;(setq browse-url-browser-function 'w3m-browse-url)
;(setq browse-url-browser-function 'browse-url-gnome-moz)
;(setq browse-url-browser-function 'browse-url-mozilla)
;(setq browse-url-browser-function 'browse-url-galeon)
;(setq browse-url-browser-function
;    (if (eq window-system 'x)
;	'browse-url-mozilla
;    )
;)
;(autoload 'browse-url "browse-url"
;	"Ask a WWW browser to show a URL." t)
;(autoload 'browse-url-at-point "browse-url"
;	"Ask a WWW browser to show a URL." t)
;(autoload 'browse-url-of-file "browse-url"
;	"Ask a WWW browser to display the current file." t)
;(autoload 'browse-url-of-buffer "browse-url"
;	"Ask a WWW browser to display the current buffer." t)
;(autoload 'browse-url-netscape-reload "browse-url"
;	"Ask a WWW browser to redisplay the current file." t)
;(autoload 'browse-url-at-mouse  "browse-url"
;	"Ask a WWW browser to display the URL clicked on." t)

;; Internet Message
;; ======================================================================

;; SEMI
;; ----------------------------------------------------------------------

;; HTML パートを表示しない
(setq mime-setup-enable-inline-html nil)
;; 大きいメッセージを送信時に分割しない
(setq mime-edit-split-message nil)

;; multipart/alternative で Plain を HTML より優先して表示
(eval-after-load "semi-setup"
  '(set-alist 'mime-view-type-subtype-score-alist '(text . plain) 10)
)
;(eval-after-load "semi-setup"
;  '(set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)
;)

(setq pgg-scheme 'gpg)
(setq pgg-default-scheme 'gpg)
(setq pgg-insert-url-function #'w3m-retrieve)

(eval-after-load "mime-edit"
  '(let ((text (assoc "text" mime-content-types)))
     (set-alist 'text "plain"
                '(("charset" "" "ISO-2022-JP" "US-ASCII"
                   "ISO-8859-1" "ISO-8859-8" "UTF-8")))
     (set-alist 'mime-content-types "text" (cdr text))
  )
)

(eval-after-load "mime-edit"
  '(set-alist 'mime-file-types "\\.txt$"
    '("text" "plain" (("charset" . "UTF-8")) "8bit" "attachment" (("filename" . file)))
  )
)

;; Wanderlust
;; ----------------------------------------------------------------------

;(autoload-if-found 'wl "wl" "Wanderlust" t)
;(autoload-if-found 'wl-draft "wl" "Write draft with Wanderlust." t)

;; Support broken MUA that generates wrong header for attached filename.
;; ----------------------------------------------------------------------

;;; ファイル名が日本語の添付ファイルをデコードする [semi-gnus-ja: 4332]
(eval-after-load "mime"
  '(defadvice mime-entity-filename
     (after eword-decode-for-broken-MUA activate)
     "Decode eworded file name for *BROKEN* MUA."
     (when (stringp ad-return-value)
       (setq ad-return-value (eword-decode-string ad-return-value t)))))

;;; ファイル名が日本語の添付ファイルをエンコードする [semi-gnus-ja: 6046]
(eval-after-load "std11"
  '(defadvice std11-wrap-as-quoted-string (before encode-string activate)
     "Encode a string."
     (require 'eword-encode)
     (ad-set-arg 0 (eword-encode-string (ad-get-arg 0)))))

;(defadvice mime-entity-fetch-field
;	   (after mime-decode-entity activate)
;	   "MIME decode fetched field of entity."
;	   (if ad-return-value
;	     (setq ad-return-value (eword-decode-string (namajis-decode ad-return-value)))))
;(defun namajis-decode (s)
;  (with-temp-buffer
;    (insert s)
;    (beginning-of-buffer)
;    (while (re-search-forward "\033$[@B][^\033]+\033([BJ]" nil t)
;	   (decode-coding-region (match-beginning 0) (match-end 0) 'junet))
;    (buffer-substring 1 (point-max))))

;;; <http://www.ya.sakura.ne.jp/~shinm/meadowbbs/msg00316.html>
;
;(defvar my-mime-filename-coding-system-for-decode
;  '(iso-2022-jp japanese-shift-jis japanese-iso-8bit))
;
;(defun my-mime-decode-filename (filename)
;  (let ((rest (eword-decode-string filename)))
;    (or (when (and my-mime-filename-coding-system-for-decode
;    (string= rest filename))
;   (let ((dcs (mapcar (function coding-system-base)
;      (detect-coding-string filename))))
;     (unless (memq 'emacs-mule dcs)
;       (let ((pcs my-mime-filename-coding-system-for-decode))
; (while pcs
;   (if (memq (coding-system-base (car pcs)) dcs)
;       (setq rest (decode-coding-string filename (car pcs))
;     pcs nil)
;     (setq pcs (cdr pcs))))))))
; rest)))
;
;(eval-after-load "mime"
;  '(defadvice mime-entity-filename (after eword-decode-for-broken-MUA activate)
;     "Decode encoded file name for BROKEN MUA."
;     (when (stringp ad-return-value)
;       (setq ad-return-value (my-mime-decode-filename ad-return-value)))))

;; http://homepage1.nifty.com/ssaychi/debian/x941.html
;(eval-after-load "mime"
;  '(defadvice mime-entity-filename (around mime-decode activate)
;     ad-do-it
;     (and ad-return-value
;      (setq ad-return-value
;        (eword-decode-string (decode-mime-charset-string
;                      ad-return-value
;                      'iso-2022-jp))))))

;; http://lists.airs.net/wl/archive/199909/msg00031.html
;(eval-after-load "mime"
;    '(defadvice mime-entity-filename (around mime-decode activate)
;	ad-do-it
;	(and ad-return-value
;	    (setq ad-return-value (eword-decode-string ad-return-value))
;	)
;    )
;)

;; Plug-ins
;; ======================================================================

;; lookup
;; ----------------------------------------------------------------------

(setq lookup-init-file "~/.lookup-el")
(setq lookup-enable-splash nil)
;(autoload-if-found 'lookup "lookup" nil t)
;(autoload-if-found 'lookup-region "lookup" nil t)
;(autoload-if-found 'lookup-pattern "lookup" nil t)

;(define-key ctl-x-map "l" 'lookup)		; C-x l - lookup
;(define-key ctl-x-map "W" 'lookup-region)	; C-x W - lookup-region
;(define-key ctl-x-map "y" 'lookup-region)	; C-x y - lookup-region
(define-key ctl-x-map "w" 'lookup-pattern)	; C-x w - lookup-pattern
;(define-key ctl-x-map "\C-y" 'lookup-pattern)	; C-x C-y - lookup-pattern

;; Lookup word: C-c l
;; Lookup region: C-u C-c l
;(autoload 'my-lookup-word "lookup" nil t)
;(global-set-key "\C-cl" 'my-lookup-word)

;; sdic
;; ----------------------------------------------------------------------

;(global-set-key "\C-cw" 'sdic-describe-word)
;(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; misc plugins
;; ----------------------------------------------------------------------

(require 'w3m-load)
(require 'mime-w3m)
(setq w3m-type 'w3mmee)
(setq w3m-fill-column -4)

;; ======================================================================

(setq pgp-version 'gpg)

;; 以下、よくわからん (古い language-env で(?)作成、あるいはパクリ)
;; ======================================================================

;; Set Wanderlust to default mailer: C-x m (compose-mail)
(autoload-if-found 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent)
)
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook
    )
)

;; Default fontset
;; ======================================================================

;(set-face-font 'default
;    '(
;	"-ricoh-ms.gothic-medium-r-normal--14-*-*-*-*-*-*-*"
;	"-*-*-medium-r-normal--14-*-*-*-*-*-jisx0213.2004-1"
;	"-*-*-medium-r-normal--14-*-*-*-*-*-jisx0213.2000-2"
;    )
;    'global
;    '(mule-fonts)
;    'prepend
;)
;(set-face-font 'bold
;    '(
;	"-ricoh-ms.gothic-medium-r-bold--14-*-*-*-*-*-*-*"
;    )
;    'global
;    '(mule-fonts)
;    'prepend
;)

;(when (featurep 'xemacs)
;    (set-face-font
;	'default "-ricoh-ms.gothic-medium-r-normal--14-*-*-*-*-*-*-*"
;	'global
;	'(mule-fonts)
;	'prepend
;    )
;    ;(set-face-font 'default "-ricoh-ms.gothic-medium-r-normal--15-*-*-*-*-*-*-*")
;    ;(set-face-font 'default "-mplus-gothic-medium-r-normal--12-*-*-*-*-*-*-*")
;)

;(setq initial-frame-alist
;    '(
;	(font . "fontset-14")
;	(width . 90)
;	(height . 50)
;	(auto-raise . t)
;    )
;)

;(setq default-frame-alist
;    '(
;	(font . "fontset-14")
;	(width . 90)
;	(height . 50)
;	(auto-raise . t)
;    )
;)

;; C プログラムの書式
;; ======================================================================
;(defun my-c-mode-common-hook ()
;   (c-set-style "linux") (setq indent-tabs-mode t) ;linux 式がいいとき
;      /usr/src/linux/Documentation/CodingStyle 参照
;   (c-set-style "k&r") ;k&r式がいいときはこれを有効にする
;   (c-set-style "gnu") ;デフォルトの設定
; )
;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;> これは load-pathにconsをつかわないで全部指定する以外に、
;> /usr/share/xemacs20/site-lisp/skkだけをdebian標準から抜く方法は
;> あるでしょうか?
;(setq load-path (delete "ぬきたいパス" load-path))

(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)

;; EOF

;; X11
;; ======================================================================

;(require 'cua)
;(cua-mode t)
;(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止
;(CUA-mode 'emacs)

;; Sync and use clipboard (and selections?)
;; http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste
;; ----------------------------------------------------------------------

;; XEmacs
(when (featurep 'xemacs)
    (setq interprogram-cut-function 'own-clipboard)
    (setq interprogram-paste-function 'get-clipboard)
)

;; Emacs 21.2.1 or later
(setq x-select-enable-clipboard t)

;(set-selection-coding-system 'compound-text-unix)
;(set-clipboard-coding-system 'utf-8-unix)

