(setq skk-server-host "127.0.0.1")
(setq skk-server-portnum 1178)
(setq skk-aux-large-jisyo "/usr/share/skk/SKK-JISYO.L")
;(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
(setq skk-jisyo-code 'utf-8)

;; 複数の Emacsen を起動して個人辞書を共有する
(setq skk-share-private-jisyo t)

;(global-set-key [hiragana-katakana] 'skk-mode)
(if (featurep 'xemacs)
    (global-set-key [henkan-mode] 'skk-mode)
    (global-set-key [henkan] 'skk-mode)
)
;(global-set-key "\C-xs" 'skk-save-jisyo)

;; JIS X 0201 カナ (いわゆる半角カナ) モードを利用する (C-q)
(setq skk-use-jisx0201-input-method t)

;; メッセージおよびエラー出力を日本語で行う
;(setq skk-japanese-message-and-error t)
;; 変換に関するログをとらない
(setq skk-keep-record nil)

(setq skk-auto-okuri-process t)

(setq skk-show-mode-show t)
;; モードの tooltip 表示はバグっていてエラーとなるのでコメントアウト
;; skk-tooltip-show-at-point: Wrong type argument: number-or-marker-p, nil
;(setq skk-show-mode-style 'tooltip)
(setq skk-show-candidates-nth-henkan-char 1)
(setq skk-henkan-number-to-display-candidates 9)
(setq skk-henkan-show-candidates-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

;; 変換時に註釈 (annotation) を表示する
(setq skk-show-annotation t)

;; tooltip で注釈/候補一覧を表示 (XEmacs 21.5)
(setq skk-show-tooltip window-system)

;; C-m では確定のみ行い, 改行しない
;; バージョンによっては, なぜかうまく動いてくれずに▼がのこってしまうらしい
(setq skk-mode-hook
    '(lambda ()
         (setq skk-egg-like-newline t)
    )
)

;; 日付表示("@")に関する設定
;; 西暦表示
(setq skk-date-ad t)
;; 数字には1バイト文字を利用
(setq skk-number-style nil)

;; 対応する閉括弧を自動的に挿入する
(setq skk-auto-insert-paren t)

;; ▼モードで BS を押したときには確定しないで前候補を表示する
(setq skk-delete-implies-kakutei nil)

(setq skk-use-look t)
(setq skk-look-recursive-search t)
(setq skk-status-indicator 'left)

;; ローマ字のルール
;; 最初ふたつは SKK のデフォルトで組込まれるもの
;; それ以外は自分で追加したものです.
;; 7行目以降は, もともと skk-input-vector にいれてあったのですが,
;; SKK 10.46 で廃止になったのにともない, ここに移しました.
(setq skk-rom-kana-rule-list
    '(
	;("nn" "n" ("ン" . "ん"))
	;("n'" nil ("ン" . "ん"))
	("wi" nil ("ヰ" . "ゐ"))
	("wu" nil ("ヴ" . "ゔ"))
	("we" nil ("ヱ" . "ゑ"))
	;("."  nil ".")
	;(","  nil ",")
	(":"  nil ":")
	(";"  nil ";")
	("?"  nil "?")
	("@"  nil "@")
	("$"  nil "$")
	("z=" nil "≒")
	("z-" nil "〜")
	("z," nil "‥")
	("z." nil "…")
	("z/" nil "・")
	("z*" nil "※")
	("z\\" nil "￥")
	("z " nil "　")
	("zh" nil "←")
	("zj" nil "↓")
	("zk" nil "↑")
	("zl" nil "→")
	("z(" nil "（")
	("z)" nil "）")
	("z<" nil "《")
	("z>" nil "》")
	("z[" nil "『")
	("z]" nil "』")
	("z{" nil "【")
	("z}" nil "】")
	("z\"" nil "“”")
	("z'" nil "‘’")
    )
)

;(global-set-key "\C-xj" 'skk-auto-fill-mode)
;(global-set-key "\C-xJ" 'register-to-point)
;(global-set-key "\C-xt" 'skk-tutorial)
;(autoload 'skk-mode "skk" nil t)
;(autoload 'skk-auto-fill-mode "skk" nil t)
;(autoload 'skk-tutorial "skk-tut" nil t)
;(autoload 'check-jisyo "skk-tools" nil t)
;(autoload 'skk-marge "skk-tools" nil t)
;(autoload 'skk-diff "skk-tools" nil t)
;(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
;(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
;(add-hook 'isearch-mode-hook
;	(function (lambda ()
;		(and (boundp 'skk-mode) skk-mode
;			(skk-isearch-mode-setup)))))
;(add-hook 'isearch-mode-end-hook
;	(function (lambda ()
;		(and (boundp 'skk-mode) skk-mode
;			(skk-isearch-mode-cleanup)
;			(skk-set-cursor-color-properly)))))

(setq skk-isearch-mode-enable nil)

(setq skk-show-tooltip t)
(setq skk-show-japanese-menu t)

;; 数字の間の - を 全角にしない
(setq skk-rom-kana-rule-list
      (cons '("-" nil skk-hyphen)
	    skk-rom-kana-rule-list))

(defun skk-hyphen (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "ー")
          ((and (<= ?0 c) (>= ?9 c)) "-")
      ((and (<= ?０ c) (>= ?９ c)) "−")
      (t "ー"))))

;; 辞書登録のとき、余計な送り仮名を送らないようにする
(setq skk-check-okurigana-on-touroku 'auto)

