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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(setq temporary-file-directory (expand-file-name "~/var/tmp"))

;; hoge.txt~ みたいなバックアップファイルを作らないようにする
;(setq backup-inhibited t)

;; Load and Save
;; ----------------------------------------------------------------------

(setq auto-save-default t)
(setq auto-save-interval 50)

;; Enable `Open Recent' menu
(exec-if-bound (recentf-mode))

;; Charachter Encoding
;; ======================================================================

;(un-define-debian)
;(un-define-debian-jisx0213)

(set-language-environment 'UTF-8)

;; 日本語 info が文字化けしないように
(auto-compression-mode t)
;; 日本語 grep
;(if (file-exists-p "/usr/bin/jgrep")
;    (setq grep-command "jgrep -n -e ")
;)

(when (locate-library "jisx0213")
    (require 'jisx0213)
)
(when (locate-library "cp5022x")
  (require 'cp5022x)

)

;; Terminal
;; ======================================================================

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq frame-title-format
  (format "%%f @ %s" (system-name))
)

(when window-system
    (tool-bar-mode -1)
    (auto-image-file-mode t)
    (mouse-wheel-mode t)
)

(setq visible-bell nil)
(setq truncate-partial-width-windows t)

(setq mouse-yank-at-point t)

(setq-default line-spacing 0)

(load-safe "bitmap-ci")

;(when (locate-library "izonmoji-mode")
;  (autoload 'izonmoji-mode "izonmoji-mode" nil t)
;  (autoload 'izonmoji-mode-on "izonmoji-mode" nil t)
;  (add-hook 'w3m-mode-hook 'izonmoji-mode-on)
;  (add-hook 'mew-message-mode-hook 'izonmoji-mode-on)
;  (add-hook 'wl-message-redisplay-hook 'izonmoji-mode-on))

;; Edit
;; ======================================================================

(setq next-line-add-newlines nil)
(setq track-eol t)
(setq-default indent-tabs-mode nil)
(load-library "paren")

;; 対応する括弧をハイライト表示
(show-paren-mode t)
;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
;(setq show-paren-style 'mixed)
;; 選択されたリージョンを色付き
(transient-mark-mode t)

;; 折り返し桁数の初期値
(setq-default fill-column 999)

;; 現在行を目立たせる
(global-hl-line-mode)
;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;; Listing kill-ring
(when (autoload-if-found 'kill-summary "kill-summary" nil t)
    (global-set-key "\M-y" 'kill-summary)
)

;; Save kill-ring and file-name-history when exit
(when (load-safe "session")
    (add-hook 'after-init-hook 'session-initialize)
)

;; 履歴数を大きくする
(setq history-length 10000)

;; word completion by words in all buffers
(load-safe "dabbrev-ja") ;; M-/
(load-safe "dabbrev-highlight")
;(global-set-key "\C-j" 'dabbrev-expand)

;; Search
;; ======================================================================

;; i-search for Japanese
(load-safe "migemo")
(setq migemo-command "/usr/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)

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
(global-font-lock-mode t)
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

(setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "firefox"
        browse-url-generic-args '("--new-tab"))

;; Internet Message
;; ======================================================================

;; SEMI
;; ----------------------------------------------------------------------

;; HTML パートを表示しない
(setq mime-setup-enable-inline-html nil)
;; 大きいメッセージを送信時に分割しない
(setq mime-edit-split-message nil)

;; charset=UTF-8 なときに Content-Transfer-Encoding: 8bit
(setq mime-transfer-level 8)

;; multipart/alternative で Plain を HTML より優先して表示
(eval-after-load "semi-setup"
  '(set-alist 'mime-view-type-subtype-score-alist '(text . plain) 10)
)
;(eval-after-load "semi-setup"
;  '(set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)
;)

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

(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)

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

;(set-face-attribute 'default nil
;                    :family "Ricty Diminished Discord"
;                    :height 160)

;(set-fontset-font (frame-parameter nil 'font)
;                  'japanese-jisx0208
;                  (cons "Ricty Diminished Discord" "iso10646-1"))
;(set-fontset-font (frame-parameter nil 'font)
;                  'japanese-jisx0212
;                  (cons "Ricty Diminished Discord" "iso10646-1"))
;(set-fontset-font (frame-parameter nil 'font)
;                  'katakana-jisx0201
;                  (cons "Ricty Diminished Discord" "iso10646-1"))

(set-face-attribute 'default nil :height 150)

;; Emacs で全世界の文字を表示・編集可能にする。 - Qiita
;; https://qiita.com/kawabata@github/items/2c4b0b689834c9c193aa
;; ----------------------------------------------------------------------

(defun set-fontset-font:around (set-fontset-font name target font-spec &optional frame add)
  "Warn if specified font is not installed."
  (if (stringp font-spec) (setq font-spec (font-spec :family font-spec)))
  (if (and (fontp font-spec)
           (null (find-font font-spec)))
      (warn "set-fontset-font: font %s is not found." (font-get font-spec :family))
    (funcall set-fontset-font name target font-spec frame add)))

(advice-add 'set-fontset-font :around #'set-fontset-font:around)

;(setq use-default-font-for-symbols nil)
(defun reset-default-fontset ()
  "Reset current fontset."
  (interactive)
  ;; reset all settings in default fontset
  ;(if (find-font (font-spec :family "Ricty Diminished Discord"))
  ;    (set-fontset-font t '(0 . #x3fffff) "Ricty Diminished Discord"))
  ;; multiple platform
  (set-fontset-font t 'latin "Ricty Diminished Discord")
  (set-fontset-font t 'greek "Noto Sans")
  (set-fontset-font t 'phonetic "Noto Sans")
  (set-fontset-font t 'coptic "Noto Sans Coptic")
  (set-fontset-font t 'coptic "Noto Sans Symbols2" nil 'append)
  (set-fontset-font t 'cyrillic "Noto Sans")
  (set-fontset-font t 'armenian "Noto Sans Armenian")
  (set-fontset-font t 'hebrew "Noto Sans Hebrew")
  (set-fontset-font t 'arabic "Noto Sans Arabic")
  (set-fontset-font t 'syriac "Noto Sans Syriac")
  (set-fontset-font t 'thaana "Noto Sans Thaana")
  (set-fontset-font t 'nko "Noto Sans NKo")
  (set-fontset-font t 'samaritan "Noto Sans Samaritan")
  (set-fontset-font t 'mandaic "Noto Sans Mandaic")
  (set-fontset-font t 'devanagari "Noto Sans Devanagari")
  (set-fontset-font t 'bengali "Noto Sans Bengali")
  (set-fontset-font t 'gurmukhi "Noto Sans Gurmukhi")
  (set-fontset-font t 'gujarati "Noto Sans Gujarati")
  (set-fontset-font t 'oriya "Noto Sans Oriya")
  (set-fontset-font t 'tamil "Noto Sans Tamil")
  (set-fontset-font t 'tamil "Noto Sans Tamil Supplement" nil 'append)
  (set-fontset-font t 'telugu "Noto Sans Telugu")
  (set-fontset-font t 'kannada "Noto Sans Kannada")
  (set-fontset-font t 'malayalam "Noto Sans Malayalam")
  (set-fontset-font t 'sinhala "Noto Sans Sinhala")
  (set-fontset-font t 'thai "Noto Sans Thai")
  (set-fontset-font t 'lao "Noto Sans Lao")
  ;(set-fontset-font t 'tibetan "Noto Sans Tibetan")
  (set-fontset-font t 'burmese "Noto Sans Myanmar")
  (set-fontset-font t 'georgian "Noto Sans Georgian")
  (set-fontset-font t 'hangul "Noto Sans CJK KR")
  (set-fontset-font t 'ethiopic "Noto Sans Ethiopic")
  (set-fontset-font t 'cherokee "Noto Sans Cherokee")
  (set-fontset-font t 'canadian-aboriginal "Noto Sans Canadian Aboriginal")
  (set-fontset-font t 'ogham "Noto Sans Ogham")
  (set-fontset-font t 'runic "Noto Sans Runic")
  (set-fontset-font t 'tagalog "Noto Sans Tagalog")
  (set-fontset-font t 'hanunoo "Noto Sans Hanunoo")
  (set-fontset-font t 'buhid "Noto Sans Buhid")
  (set-fontset-font t 'tagbanwa "Noto Sans Tagbanwa")
  (set-fontset-font t 'khmer "Noto Sans Khmer")
  (set-fontset-font t 'mongolian "Noto Sans Mongolian")
  (set-fontset-font t 'limbu "Noto Sans Limbu")
  (set-fontset-font t 'tai-le "Noto Sans Tai Le")
  (set-fontset-font t 'tai-lue "Noto Sans NewTaiLue")
  (set-fontset-font t 'buginese "Noto Sans Buginese")
  (set-fontset-font t 'tai-tham "Noto Sans Tai Tham")
  (set-fontset-font t 'balinese "Noto Sans Balinese")
  (set-fontset-font t 'sundanese "Noto Sans Sundanese")
  (set-fontset-font t 'vedic "Noto Sans Devanagari")
  (set-fontset-font t 'symbol "Noto Sans CJK JP")
  (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
  (set-fontset-font t 'symbol "Noto Sans" nil 'append)
  (set-fontset-font t 'symbol "Noto Sans Math" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
  (set-fontset-font t 'braille "Noto Sans Symbols2")
  (set-fontset-font t 'batak "Noto Sans Batak")
  (set-fontset-font t 'lepcha "Noto Sans Lepcha")
  (set-fontset-font t 'ol-chiki "Noto Sans Ol Chiki")
  (set-fontset-font t 'glagolitic "Noto Sans Glagolitic")
  (set-fontset-font t 'tifinagh "Noto Sans Tifinagh")
  (set-fontset-font t 'han "Noto Sans CJK JP")
  (set-fontset-font t 'ideographic-description "Noto Sans CJK JP")
  (set-fontset-font t 'cjk-misc "Noto Sans CJK JP")
  (set-fontset-font t 'kana "Noto Sans CJK JP")
  (set-fontset-font t 'bopomofo "Noto Sans CJK TC")
  (set-fontset-font t 'kanbun "Noto Sans CJK JP")
  (set-fontset-font t 'yi "Noto Sans Yi")
  (set-fontset-font t 'lisu "Noto Sans Lisu")
  (set-fontset-font t 'vai "Noto Sans Vai")
  (set-fontset-font t 'bamum "Noto Sans Bamum")
  (set-fontset-font t 'syloti-nagri "Noto Sans Syloti Nagri")
  (set-fontset-font t 'north-indic-number "Noto Sans Devanagari")
  (set-fontset-font t 'phags-pa "Noto Sans Phags Pa")
  (set-fontset-font t 'saurashtra "Noto Sans Saurashtra")
  (set-fontset-font t 'kayah-li "Noto Sans Kayah Li")
  (set-fontset-font t 'rejang "Noto Sans Rejang")
  (set-fontset-font t 'javanese "Noto Sans Javanese")
  (set-fontset-font t 'cham "Noto Sans Cham")
  (set-fontset-font t 'tai-viet "Noto Sans Tai Viet")
  (set-fontset-font t 'meetei-mayek "Noto Sans Meetei Mayek")
  (set-fontset-font t 'vertical-form "Noto Sans CJK JP")
  ;; (set-fontset-font t '(#xfe50 . #xfe6b) "Noto Sans CJK JP") ; symbol
  (set-fontset-font t '(#xfff9 . #xfffb) "Noto Sans Symbols2") ; nil
  (set-fontset-font t 'linear-b "Noto Sans Linear B")
  (set-fontset-font t 'aegean-number "Noto Sans Linear B")
  (set-fontset-font t 'ancient-greek-number "Noto Sans Symbols2")
  (set-fontset-font t 'ancient-symbol "Noto Sans Symbols2")
  (set-fontset-font t 'phaistos-disc "Noto Sans Symbols2")
  (set-fontset-font t 'lycian "Noto Sans Lycian")
  (set-fontset-font t 'carian "Noto Sans Carian")
  (set-fontset-font t 'old-italic "Noto Sans Old Italic")
  (set-fontset-font t 'gothic "Noto Sans Gothic")
  (set-fontset-font t 'old-permic "Noto Sans Old Permic")
  (set-fontset-font t 'ugaritic "Noto Sans Ugaritic")
  (set-fontset-font t 'old-persian "Noto Sans OldPersian")
  (set-fontset-font t 'deseret "Noto Sans Deseret")
  (set-fontset-font t 'shavian "Noto Sans Shavian")
  (set-fontset-font t 'osmanya "Noto Sans Osmanya")
  (set-fontset-font t 'osage "Noto Sans Osage")
  (set-fontset-font t 'elbasan "Noto Sans Elbasan")
  (set-fontset-font t 'caucasian-albanian "Noto Sans CaucAlban")
  (set-fontset-font t 'linear-a "Noto Sans Linear A")
  (set-fontset-font t 'cypriot-syllabary "Noto Sans Cypriot")
  (set-fontset-font t 'aramaic "Noto Sans ImpAramaic")
  (set-fontset-font t 'palmyrene "Noto Sans Palmyrene")
  (set-fontset-font t 'nabataean "Noto Sans Nabataean")
  (set-fontset-font t 'hatran "Noto Sans Hatran")
  (set-fontset-font t 'phoenician "Noto Sans Phoenician")
  (set-fontset-font t 'lydian "Noto Sans Lydian")
  (set-fontset-font t 'meroitic "Noto Sans Meroitic")
  (set-fontset-font t 'kharoshthi "Noto Sans Kharoshthi")
  (set-fontset-font t 'old-south-arabian "Noto Sans OldSouArab")
  (set-fontset-font t 'old-north-arabian "Noto Sans OldNorArab")
  (set-fontset-font t 'manichaean "Noto Sans Manichaean")
  (set-fontset-font t 'avestan "Noto Sans Avestan")
  (set-fontset-font t 'inscriptional-parthian "Noto Sans Inscriptional Parthian")
  (set-fontset-font t 'inscriptional-pahlavi "Noto Sans Inscriptional Pahlavi")
  (set-fontset-font t 'psalter-pahlavi "Noto Sans PsaPahlavi")
  (set-fontset-font t 'old-turkic "Noto Sans Old Turkic")
  (set-fontset-font t 'old-hungarian "Noto Sans OldHung")
  (set-fontset-font t 'hanifi-rohingya "Noto Sans Hanifi Rohingya")
  ;; Emacs 29.1: error: Invalid script or charset name: rumi-number
  ;;(set-fontset-font t 'rumi-number "Noto Sans Symbols2")
  (set-fontset-font t 'old-sogdian "Noto Sans OldSogdian")
  (set-fontset-font t 'sogdian "Noto Sans Sogdian")
  (set-fontset-font t 'elymaic "Noto Sans Elymaic")
  (set-fontset-font t 'brahmi "Noto Sans Brahmi")
  (set-fontset-font t 'kaithi "Noto Sans Kaithi")
  (set-fontset-font t 'sora-sompeng "Noto Sans Sora Sompeng")
  (set-fontset-font t 'chakma "Noto Sans Chakma")
  (set-fontset-font t 'mahajani "Noto Sans Mahajani")
  (set-fontset-font t 'sharada "Noto Sans Sharada")
  ;; Emacs 29.1: error: Invalid script or charset name: sinhala-archaic-number
  ;(set-fontset-font t 'sinhala-archaic-number "Noto Sans Sinhala")
  (set-fontset-font t 'khojki "Noto Sans Khojki")
  (set-fontset-font t 'multani "Noto Sans Multani")
  (set-fontset-font t 'khudawadi "Noto Sans Khudawadi")
  (set-fontset-font t 'grantha "Noto Sans Grantha")
  (set-fontset-font t 'newa "Noto Sans Newa")
  (set-fontset-font t 'tirhuta "Noto Sans Tirhuta")
  (set-fontset-font t 'siddham "Noto Sans Siddham")
  (set-fontset-font t 'modi "Noto Sans Modi")
  (set-fontset-font t 'takri "Noto Sans Takri")
  (set-fontset-font t 'ahom "Noto Serif Ahom")
  (set-fontset-font t 'dogra "Noto Serif Dogra")
  (set-fontset-font t 'warang-citi "Noto Sans WarangCiti")
  (set-fontset-font t 'zanabazar-square "Noto Sans Zanabazar")
  (set-fontset-font t 'soyombo "Noto Sans Soyombo")
  (set-fontset-font t 'pau-cin-hau "Noto Sans PauCinHau")
  (set-fontset-font t 'bhaiksuki "Noto Sans Bhaiksuki")
  (set-fontset-font t 'marchen "Noto Sans Marchen")
  (set-fontset-font t 'masaram-gondi "Noto Sans Masaram Gondi")
  (set-fontset-font t 'gunjala-gondi "Noto Sans Gunjala Gondi")
  (set-fontset-font t 'cuneiform "Noto Sans Cuneiform")
  ;; Emacs 29.1: error: Invalid script or charset name: cuneiform-numbers-and-punctuation
  ;(set-fontset-font t 'cuneiform-numbers-and-punctuation "Noto Sans Cuneiform")
  (set-fontset-font t 'egyptian "Noto Sans EgyptHiero")
  (set-fontset-font t 'anatolian "Noto Sans AnatoHiero")
  (set-fontset-font t 'mro "Noto Sans Mro")
  (set-fontset-font t 'bassa-vah "Noto Sans Bassa Vah")
  (set-fontset-font t 'pahawh-hmong "Noto Sans Pahawh Hmong")
  (set-fontset-font t 'miao "Noto Sans Miao")
  (set-fontset-font t 'tangut "Noto Serif Tangut")
  ;; Emacs 29.1: error: Invalid script or charset name: tangut-components
  ;(set-fontset-font t 'tangut-components "Noto Serif Tangut")
  (set-fontset-font t '(#x16fe0 . #x16fe0) "Noto Serif Tangut")
  (set-fontset-font t 'duployan-shorthand "Noto Sans Duployan")
  (set-fontset-font t 'byzantine-musical-symbol "Noto Music")
  (set-fontset-font t 'musical-symbol "Noto Music")
  (set-fontset-font t 'ancient-greek-musical-notation "Noto Music")
  (set-fontset-font t 'mayan-numeral "Noto Sans Mayan Numerals")
  (set-fontset-font t 'tai-xuan-jing-symbol "Noto Sans Symbols2")
  (set-fontset-font t 'counting-rod-numeral "Noto Sans Symbols2")
  (set-fontset-font t 'mathematical "Noto Sans Math")
  (set-fontset-font t 'wancho "Noto Sans Wancho")
  (set-fontset-font t 'mende-kikakui "Noto Sans Mende Kikakui")
  (set-fontset-font t 'adlam "Noto Sans Adlam")
  (set-fontset-font t 'indic-siyaq-number "Noto Sans Indic Siyaq Numbers")
  (set-fontset-font t '(#x1ee00 . #x1eeff) "Noto Sans Math") ; arabic
  (set-fontset-font t 'mahjong-tile "Noto Sans Symbols2")
  (set-fontset-font t 'domino-tile "Noto Sans Symbols2")
  (set-fontset-font t 'playing-cards "Noto Sans Symbols2")
  ;; non Noto fonts
  ;(set-fontset-font t 'kana "UniHentaiKana" nil 'append)
  ;(set-fontset-font t 'latin "Iosevka" nil 'append)
  ;(set-fontset-font t 'symbol "Iosevka" nil 'append)
  ;; Nerd Font (defined thru -#xfd46)
  ;(set-fontset-font t '( #xe000 .  #xf136) "Inconsolata Nerd Font")
  ;;
  (when (eq window-system 'w32)
    (set-fontset-font t 'bengali    "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'gurmukhi   "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'gujarati   "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'telugu     "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'kannada    "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'malayalam  "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'vedic      "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'devanagari "Nirmala UI" nil 'prepend)
    (set-fontset-font t 'brahmi     "Segoe UI Historic" nil 'prepend)
    (set-fontset-font t 'georgian   "Segoe UI" nil 'prepend)
    (set-fontset-font t '( #x0300 .  #x1cff) "Arial" nil 'append)
    (set-fontset-font t '( #x2000 .  #x27bf) "Segoe UI Symbol" nil 'append)
    (set-fontset-font t '( #x2e00 .  #xdfff) "花園明朝 A Regular" nil 'append)
    (set-fontset-font t '( #xfe0e .  #xfe0f) "Segoe UI Emoji")
    (set-fontset-font t '( #xf137 .  #xf69d) "花園明朝 A Regular")
    (set-fontset-font t '(#x1b000 . #x1b000) "Meiryo UI")
    (set-fontset-font t '(#x1f300 . #x1f9ff) "Segoe UI Emoji" nil 'append)
    (set-fontset-font t '(#x1fa70 . #x1fbff) "Segoe UI Emoji" nil 'append)
    (set-fontset-font t '(#x1f900 . #x1f9e0) "Segoe UI Emoji" nil 'append)
    (set-fontset-font t '(#x20000 . #x2a6ff) "花園明朝 B Regular")
    (set-fontset-font t '(#x2a700 . #x2fffd) "花園明朝 C Regular")
  )
  (when (not (eq system-type 'windows-nt))
    ;(set-fontset-font t '( #x2e00 .  #xdfff) "Hanazono Mincho A Regular" nil 'append)
    ;(set-fontset-font t '( #xf137 .  #xf69d) "Hanazono Mincho A Regular")
    ;(set-fontset-font t '(#x20000 . #x2a6ff) "Hanazono Mincho B Regular")
    ;(set-fontset-font t '(#x2a700 . #x2fffd) "Hanazono Mincho C Regular")
  )
)

(reset-default-fontset)

;; X11
;; ======================================================================

;(require 'cua)
;(cua-mode t)
;(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止
;(CUA-mode 'emacs)

;; Sync and use clipboard (and selections?)
;; http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste
;; ----------------------------------------------------------------------

(setq x-select-enable-clipboard t)

;(set-selection-coding-system 'compound-text-unix)
;(set-clipboard-coding-system 'utf-8-unix)

