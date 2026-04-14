# Global Copilot instructions

## 基本

* 会話は日本語で、ソースコード中などのコメントなどは英語で
* 選択肢:
    * 比較と推奨を提示
    * セキュリティ・保守性 (単純さ)・互換性・性能・主流を優先
    * 決める前に確認させて
* 検討が一通り完了した後に批判的な目線で自己レビュー
* 質問・指摘は非難・取り消し・指示ではない
  <!-- Claude Code なら `/btw` で可能 -->
* 実装の追加・変更時は対応する文書・テストも作成

## コミットログ

* 英語で
* 概要を簡潔に
* Summary / Test Plan といった AI 臭いテンプレートは書かない
* 性能改善ならベースブランチとの比較表をつけて
* コミット前に確認させて

<!--
* 修正対象があるなら `Closes #<issue>` を付けて
* ブランチ名に `feat/`, `perf/` 等分かりやすい接頭辞をつけて
-->

## プルリクエスト (マージリクエスト)

* 出す前に確認させて

<!--
Problem with COPILOT_CUSTOM_INSTRUCTIONS_DIRS · Issue #1433 · github/copilot-cli
https://github.com/github/copilot-cli/issues/1433
-->
