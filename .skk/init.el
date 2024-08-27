;; DDSKK: .skk/init.el
;; ======================================================================

(setq skk-show-japanese-menu t)

(setq skk-isearch-mode-enable nil)

;; メッセージおよびエラー出力を日本語で行う
;(setq skk-japanese-message-and-error t)
;; 変換に関するログをとらない
(setq skk-keep-record nil)

(setq skk-show-tooltip t)
(setq skk-show-mode-show t)
;; モードの tooltip 表示はバグっていてエラーとなるのでコメントアウト
;; skk-tooltip-show-at-point: Wrong type argument: number-or-marker-p, nil
;(setq skk-show-mode-style 'tooltip)
(setq skk-status-indicator 'left)
(setq skk-show-candidates-nth-henkan-char 1)
(setq skk-henkan-number-to-display-candidates 9)
(setq skk-henkan-show-candidates-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

;; 変換時に註釈 (annotation) を表示する
(setq skk-show-annotation t)

;; 辞書
;; ======================================================================

(setq skk-server-host "127.0.0.1")
(setq skk-server-portnum 1178)
(setq skk-aux-large-jisyo "/usr/share/skk/SKK-JISYO.L")
;(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")

(setq skk-jisyo-code 'utf-8)
;; 複数の Emacsen を起動して個人辞書を共有する
(setq skk-share-private-jisyo t)

;; 辞書登録のとき、余計な送り仮名を送らないようにする
(setq skk-check-okurigana-on-touroku 'auto)

;; 入力・変換
;; ======================================================================

;(global-set-key [hiragana-katakana] 'skk-mode)
(if (featurep 'xemacs)
    (global-set-key [henkan-mode] 'skk-mode)
    (global-set-key [henkan] 'skk-mode)
)
;(global-set-key "\C-xs" 'skk-save-jisyo)
;(global-set-key "\C-xj" 'skk-auto-fill-mode)
;(global-set-key "\C-xJ" 'register-to-point)
;(global-set-key "\C-xt" 'skk-tutorial)

;; C-m では確定のみ行い, 改行しない
;; バージョンによっては, なぜかうまく動いてくれずに▼がのこってしまうらしい
(setq skk-mode-hook
    '(lambda ()
         (setq skk-egg-like-newline t)
    )
)

;; JIS X 0201 カナ (いわゆる半角カナ) モードを利用する (C-q)
(setq skk-use-jisx0201-input-method t)

;; look(1) による英単語の補完
(setq skk-use-look t)
(setq skk-look-recursive-search t)

(setq skk-auto-okuri-process t)

;; ▼モードで BS を押したときには確定しないで前候補を表示する
(setq skk-delete-implies-kakutei nil)

;; 日付表示("@")に関する設定
;; 西暦表示
(setq skk-date-ad t)
;; 数字には1バイト文字を利用
(setq skk-number-style nil)

;; 対応する閉括弧を自動的に挿入する
(setq skk-auto-insert-paren t)

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
