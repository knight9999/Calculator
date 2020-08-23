# イントロ

PureScript + Halogenで実装した電卓アプリ

参考にしたのは、

React.jsで実装された電卓アプリ
https://github.com/ahfarmer/calculator

React.js版電卓のプレイ
https://ahfarmer.github.io/calculator/


# インストールとビルド

```
$ npm install
$ npx spago install
$ npm run build
```

# アプリの開き方

```
$ npm run serve
```

または、

`public/index.html`を静的なファイルとしてブラウザ(Chrome, Firefoxなど)で開きます。


# テスト

$ npm test


# 開発ノート（メモ）

## 方針

スロットを使ってボタンを作る
https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/components-inputs/src/Container.purs

この場合、スロットからのQueryがないので、absurdが使われている。

ボタンを押した時のQueryを処理するためには、

https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/components/src/Container.purs

のように、HandledButtonの処理をslotの第四パラメータに使う

## 2020/08/22

OperationとCommandが複雑なので、一本化する

```
data Operation = Plus | Minus | Prod | Div | Equal | Nop
data Command = AC | PlusMinus | Percent | Div | Prod | Plus | Minus | Dot | Equal | Num Int
```

これを

```
data Operation = Plus | Minus | Prod | Div | Equal | Nop
data Command = AC | PlusMinus | Percent | Dot | Equal | Operation | Num Int
```

とする。

## 2020/08/23

ButtonのViewを分離する
CalculatorApp.pursをCalculator.pursに変更する

Modelを保存するリポジトリ機能を実装する？

タイトルをPureScript Calculatorにする。

Calculatorのロジックを取り出して、単体でテスト出来るようにする。


