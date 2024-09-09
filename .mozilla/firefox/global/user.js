// ln -s ../global/user.pref ./profilename/user.pref

// http://www.hackermusings.com/2012/05/firefoxs-graphics-performance-on-x11/
//user_pref("gfx.xrender.enabled", false);

// キャレット (文字入力エリア内のカーソル) の点滅速度
user_pref("ui.caretBlinkTime", 250);

// URL バーの URL を省略表示しない
user_pref("browser.urlbar.trimURLs", false);
// URL バーのドロップダウンメニューに表示する URL の数
user_pref("browser.urlbar.maxRichResults", 30);
// URL バーの入力補完を自動的に適用しない
//user_pref("browser.urlbar.autoFill", false);
// URL バーにフォーカスを移したときにドロップダウンメニューを表示しない
// [Privacy & Search] Category > [Address Bar] Clause > [Shortcuts] Check
// (↓キーで開く)
user_pref("browser.urlbar.suggest.topsites", false);

// URL バーのデザインを Firefox 74 以前に戻す
// これでないと履歴の表示幅が狭くなって URL が見切れる。
// https://www.userchrome.org/megabar-styling-firefox-address-bar.html
// obsolete?
//user_pref("browser.urlbar.update1", false);

// URL バーでの検索を無効化
user_pref("keyword.enabled", false);

// Backspace でページスクロールアップ
user_pref("browser.backspace_action", 1);

// Ctrl + マウスホイール回転で何もしない
user_pref("mousewheel.with_control.action", 0);

// 中クリックでクリップボードの URL を開かない
user_pref("middlemouse.contentLoadURL", false);

// 右クリック禁止ページを回避（JavaScript の詳細設定）
user_pref("dom.event.contextmenu.enabled", false);

// 国際化ドメイン名を punycode のまま表示
user_pref("network.IDN_show_punycode", true);

// タブを現在のタブの後に開く
user_pref("browser.tabs.insertAfterCurrent", true);
// タブをダブルクリックで閉じる
user_pref("browser.tabs.closeTabByDblclick", true);
// 最後のタブを閉じたとき、ウィンドウ自体は閉じない
user_pref("browser.tabs.closeWindowWithLastTab", false);

// [閉じたタブを復元] の記憶するタブの数
user_pref("browser.sessionstore.max_tabs_undo", 1000);

// Always ask you where to save files
user_pref("browser.download.useDownloadDir", false);

// テキストエリアへの中ボタンクリックでクリップボードでなくセレクションをペースト
// This breaks copy-and-paste behavior on Google Docs, Twitter, Facebook
//user_pref("dom.event.clipboardevents.enabled", false);

// 署名なしの拡張を有効化
//user_pref("xpinstall.signatures.required", false);

// ======================================================================

//user_pref("layout.css.devPixelsPerPx", "1.2");
//
//user_pref("font.size.variable.ar",19);
//user_pref("font.size.variable.el",19);
//user_pref("font.size.variable.he",19);
//user_pref("font.size.variable.ja",19);
//user_pref("font.size.variable.ko",19);
//user_pref("font.size.variable.th",19);
//user_pref("font.size.variable.x-armn",19);
//user_pref("font.size.variable.x-beng",19);
//user_pref("font.size.variable.x-cans",19);
//user_pref("font.size.variable.x-cyrillic",19);
//user_pref("font.size.variable.x-devanagari",19);
//user_pref("font.size.variable.x-ethi",19);
//user_pref("font.size.variable.x-geor",19);
//user_pref("font.size.variable.x-gujr",19);
//user_pref("font.size.variable.x-guru",19);
//user_pref("font.size.variable.x-khmr",19);
//user_pref("font.size.variable.x-knda",19);
//user_pref("font.size.variable.x-math",19);
//user_pref("font.size.variable.x-mlym",19);
//user_pref("font.size.variable.x-orya",19);
//user_pref("font.size.variable.x-sinh",19);
//user_pref("font.size.variable.x-tamil",19);
//user_pref("font.size.variable.x-telu",19);
//user_pref("font.size.variable.x-tibt",19);
//user_pref("font.size.variable.x-unicode",19);
//user_pref("font.size.variable.x-western",19);
//user_pref("font.size.variable.zh-CN",19);
//user_pref("font.size.variable.zh-HK",19);
//user_pref("font.size.variable.zh-TW",19);
//
//user_pref("font.size.fixed.ar", 19);
//user_pref("font.size.fixed.el", 19);
//user_pref("font.size.fixed.he", 19);
//user_pref("font.size.fixed.ja", 19);
//user_pref("font.size.fixed.ko", 19);
//user_pref("font.size.fixed.th", 19);
//user_pref("font.size.fixed.x-armn", 19);
//user_pref("font.size.fixed.x-beng", 19);
//user_pref("font.size.fixed.x-cans", 19);
//user_pref("font.size.fixed.x-cyrillic", 19);
//user_pref("font.size.fixed.x-devanagari", 19);
//user_pref("font.size.fixed.x-ethi", 19);
//user_pref("font.size.fixed.x-geor", 19);
//user_pref("font.size.fixed.x-gujr", 19);
//user_pref("font.size.fixed.x-guru", 19);
//user_pref("font.size.fixed.x-khmr", 19);
//user_pref("font.size.fixed.x-knda", 19);
//user_pref("font.size.fixed.x-math", 19);
//user_pref("font.size.fixed.x-mlym", 19);
//user_pref("font.size.fixed.x-orya", 19);
//user_pref("font.size.fixed.x-sinh", 19);
//user_pref("font.size.fixed.x-tamil", 19);
//user_pref("font.size.fixed.x-telu", 19);
//user_pref("font.size.fixed.x-tibt", 19);
//user_pref("font.size.fixed.x-unicode", 19);
//user_pref("font.size.fixed.x-western", 19);
//user_pref("font.size.fixed.zh-CN", 19);
//user_pref("font.size.fixed.zh-HK", 19);
//user_pref("font.size.fixed.zh-TW", 19);
//
//user_pref("font.minimum-size.ar", 0);
//user_pref("font.minimum-size.el", 0);
//user_pref("font.minimum-size.he", 0);
//user_pref("font.minimum-size.ja", 0);
//user_pref("font.minimum-size.ko", 0);
//user_pref("font.minimum-size.th", 0);
//user_pref("font.minimum-size.x-armn", 0);
//user_pref("font.minimum-size.x-beng", 0);
//user_pref("font.minimum-size.x-cans", 0);
//user_pref("font.minimum-size.x-cyrillic", 0);
//user_pref("font.minimum-size.x-devanagari", 0);
//user_pref("font.minimum-size.x-ethi", 0);
//user_pref("font.minimum-size.x-geor", 0);
//user_pref("font.minimum-size.x-gujr", 0);
//user_pref("font.minimum-size.x-guru", 0);
//user_pref("font.minimum-size.x-khmr", 0);
//user_pref("font.minimum-size.x-knda", 0);
//user_pref("font.minimum-size.x-math", 0);
//user_pref("font.minimum-size.x-mlym", 0);
//user_pref("font.minimum-size.x-orya", 0);
//user_pref("font.minimum-size.x-sinh", 0);
//user_pref("font.minimum-size.x-tamil", 0);
//user_pref("font.minimum-size.x-telu", 0);
//user_pref("font.minimum-size.x-tibt", 0);
//user_pref("font.minimum-size.x-unicode", 0);
//user_pref("font.minimum-size.x-western", 0);
//user_pref("font.minimum-size.zh-CN", 0);
//user_pref("font.minimum-size.zh-HK", 0);
//user_pref("font.minimum-size.zh-TW", 0);

// ======================================================================


