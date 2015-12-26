// ln -s ../global/user.pref ./profilename/user.pref

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

