// ln -s ../global/user.pref ./profilename/user.pref

// http://www.hackermusings.com/2012/05/firefoxs-graphics-performance-on-x11/
user_pref("gfx.xrender.enabled", false);

// Back Space でページスクロールアップ
user_pref("browser.backspace_action", 1);

// Ctrl + マウスホイール回転で何もしない
user_pref("mousewheel.with_control.action", 0);

// 右クリック禁止ページを回避（JavaScript の詳細設定）
user_pref("dom.event.contextmenu.enabled", false);

// フレームなページで強制的にリサイズ可能に
user_pref("layout.frames.force_resizability", true);

// Twitter Web UI 表示時の不具合の回避
// https://twitter.com/d_toybox/status/637088309072625664
//user_pref("layout.interruptible-reflow.enabled", false);

// 「タブを閉じる」ボタンの表示方法
// 0:アクティブなタブのみ *1:全てのタブ 2:表示しない 3:タブバーの右端
user_pref("browser.tabs.closeButtons", 2);

// 検索欄に入力時に表示される検索サイト選択ボタンを非表示
user_pref("browser.search.showOneOffButtons", false);

// テキストエリアへの中ボタンクリックでクリップボードでなくセレクションをペースト
user_pref("dom.event.clipboardevents.enabled", false);

// 署名なしの拡張を有効化
user_pref("xpinstall.signatures.required", false);

//user_pref("layers.acceleration.force-enabled", true);

// ======================================================================

user_pref("layout.css.devPixelsPerPx", 1.2);

user_pref("font.size.variable.ar",19);
user_pref("font.size.variable.el",19);
user_pref("font.size.variable.he",19);
user_pref("font.size.variable.ja",19);
user_pref("font.size.variable.ko",19);
user_pref("font.size.variable.th",19);
user_pref("font.size.variable.x-armn",19);
user_pref("font.size.variable.x-beng",19);
user_pref("font.size.variable.x-cans",19);
user_pref("font.size.variable.x-cyrillic",19);
user_pref("font.size.variable.x-devanagari",19);
user_pref("font.size.variable.x-ethi",19);
user_pref("font.size.variable.x-geor",19);
user_pref("font.size.variable.x-gujr",19);
user_pref("font.size.variable.x-guru",19);
user_pref("font.size.variable.x-khmr",19);
user_pref("font.size.variable.x-knda",19);
user_pref("font.size.variable.x-math",19);
user_pref("font.size.variable.x-mlym",19);
user_pref("font.size.variable.x-orya",19);
user_pref("font.size.variable.x-sinh",19);
user_pref("font.size.variable.x-tamil",19);
user_pref("font.size.variable.x-telu",19);
user_pref("font.size.variable.x-tibt",19);
user_pref("font.size.variable.x-unicode",19);
user_pref("font.size.variable.x-western",19);
user_pref("font.size.variable.zh-CN",19);
user_pref("font.size.variable.zh-HK",19);
user_pref("font.size.variable.zh-TW",19);

user_pref("font.size.fixed.ar", 19);
user_pref("font.size.fixed.el", 19);
user_pref("font.size.fixed.he", 19);
user_pref("font.size.fixed.ja", 19);
user_pref("font.size.fixed.ko", 19);
user_pref("font.size.fixed.th", 19);
user_pref("font.size.fixed.x-armn", 19);
user_pref("font.size.fixed.x-beng", 19);
user_pref("font.size.fixed.x-cans", 19);
user_pref("font.size.fixed.x-cyrillic", 19);
user_pref("font.size.fixed.x-devanagari", 19);
user_pref("font.size.fixed.x-ethi", 19);
user_pref("font.size.fixed.x-geor", 19);
user_pref("font.size.fixed.x-gujr", 19);
user_pref("font.size.fixed.x-guru", 19);
user_pref("font.size.fixed.x-khmr", 19);
user_pref("font.size.fixed.x-knda", 19);
user_pref("font.size.fixed.x-math", 19);
user_pref("font.size.fixed.x-mlym", 19);
user_pref("font.size.fixed.x-orya", 19);
user_pref("font.size.fixed.x-sinh", 19);
user_pref("font.size.fixed.x-tamil", 19);
user_pref("font.size.fixed.x-telu", 19);
user_pref("font.size.fixed.x-tibt", 19);
user_pref("font.size.fixed.x-unicode", 19);
user_pref("font.size.fixed.x-western", 19);
user_pref("font.size.fixed.zh-CN", 19);
user_pref("font.size.fixed.zh-HK", 19);
user_pref("font.size.fixed.zh-TW", 19);

user_pref("font.minimum-size.ar", 0);
user_pref("font.minimum-size.el", 0);
user_pref("font.minimum-size.he", 0);
user_pref("font.minimum-size.ja", 0);
user_pref("font.minimum-size.ko", 0);
user_pref("font.minimum-size.th", 0);
user_pref("font.minimum-size.x-armn", 0);
user_pref("font.minimum-size.x-beng", 0);
user_pref("font.minimum-size.x-cans", 0);
user_pref("font.minimum-size.x-cyrillic", 0);
user_pref("font.minimum-size.x-devanagari", 0);
user_pref("font.minimum-size.x-ethi", 0);
user_pref("font.minimum-size.x-geor", 0);
user_pref("font.minimum-size.x-gujr", 0);
user_pref("font.minimum-size.x-guru", 0);
user_pref("font.minimum-size.x-khmr", 0);
user_pref("font.minimum-size.x-knda", 0);
user_pref("font.minimum-size.x-math", 0);
user_pref("font.minimum-size.x-mlym", 0);
user_pref("font.minimum-size.x-orya", 0);
user_pref("font.minimum-size.x-sinh", 0);
user_pref("font.minimum-size.x-tamil", 0);
user_pref("font.minimum-size.x-telu", 0);
user_pref("font.minimum-size.x-tibt", 0);
user_pref("font.minimum-size.x-unicode", 0);
user_pref("font.minimum-size.x-western", 0);
user_pref("font.minimum-size.zh-CN", 0);
user_pref("font.minimum-size.zh-HK", 0);
user_pref("font.minimum-size.zh-TW", 0);

// ======================================================================

// Firefox 内臓のセッション復元機能を使用
user_pref("extensions.tabmix.sessions.manager", false);

// [閉じたタブを復元] の記憶するタブの数
user_pref("browser.sessionstore.max_tabs_undo", 100);

// 新しいタブを開くボタンを表示しない
user_pref("extensions.tabmix.newTabButton", false);
// タブを閉じるボタンをタブにカーソルが載っている間だけ表示
user_pref("extensions.tabmix.tabs.closebuttons", 2);

// 新しいタブに空白ページを表示する
user_pref("extensions.tabmix.loadOnNewTab.type", 0);

// 開くタブの位置を現在タブの隣にする
user_pref("extensions.tabmix.openNewTabNext", true);
user_pref("extensions.tabmix.openTabNext", true);
user_pref("browser.tabs.insertRelatedAfterCurrent", false);

// 最後のタブを閉じたとき、ウィンドウ自体は閉じない
user_pref("browser.closeWindowWithLastTab", false);

