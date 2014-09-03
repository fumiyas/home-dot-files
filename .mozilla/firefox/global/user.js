// ln -s ../user.pref ./profilename/user.pref

user_pref("mousewheel.with_control.action", 0);

// 右クリック禁止ページを回避（JavaScript の詳細設定）
user_pref("dom.event.contextmenu.enabled", false);

// フレームなページで強制的にリサイズ可能に
user_pref("layout.frames.force_resizability", true);

// 「タブを閉じる」ボタンの表示方法
// 0:アクティブなタブのみ *1:全てのタブ 2:表示しない 3:タブバーの右端
user_pref("browser.tabs.closeButtons", 2);

// テキストエリアへの中ボタンクリックでクリップボードでなくセレクションをペースト
user_pref("dom.event.clipboardevents.enabled", false);

