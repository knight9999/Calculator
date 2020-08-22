# イントロ

PureScript + Halogenで実装した電卓アプリ

参考にしたのは、

React.jsで実装された電卓アプリ
https://github.com/ahfarmer/calculator

React.js版電卓のプレイ
https://ahfarmer.github.io/calculator/

# 開発ノート

スロットを使ってボタンを作る
https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/components-inputs/src/Container.purs

この場合、スロットからのQueryがないので、absurdが使われている。

ボタンを押した時のQueryを処理するためには、

https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/components/src/Container.purs

のように、HandledButtonの処理をslotの第四パラメータに使う