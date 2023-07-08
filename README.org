* Fortran AtCoder用ライブラリー
** 目的
(私が)AtCoder の問題を解くための Fortran 用のライブラリ.
GFortran を想定.
** 使い方
- ~src/~ のコードをコピペしてください.
- (基本的に私が Fortran で AtCoder([[https://atcoder.jp/]]) に挑むためのコード類なので, どこになにがあるかが分かりづらいと思います...)
** 何がある?
よく使うものも全く使わないものもゴチャ混ぜ.
*** utilities
便利になりそうな関数とかユーザ定義型たち.
座標圧縮と拡張ユークリッドの互除法はたまによく使う.
**** swap
コピペするよりも自分で書いた早い.
**** is_sorted
使ったことない...
**** 座標圧縮(compress)
Ordered Map の変わりに使ったり...
~binary_search_m~ と ~merge_sort_m~ が必要.
- =integer(int32)= にしか対応していない.
- 分類は ~data structure~ の方では?
**** 拡張ユークリッドの互除法(extend euclid)
mod M の下での逆元を求めたりとか...
- 分類は ~math~ の方では?
  + 更に言うと, 型 ~modint~ の中にあるべきでは?
**** polymorphic class(*)
よくわからない.
多分消す.
*** sorting
安定ソートな merge-sort だけよく使う.
**** insertion-sort, selection-sort, bubble-sort
とりあえず作ってみたやつら.
O(N^2) だから役には立たない.
**** merge-sort
マージソートは安定ソート(同じ値のときに, 元の並び順を変えない.).
よく使っているので, 実装はおそらく正しい.
merge_sort_with_key では第一引数を key にして第二引数をソートできる.
**** heap-sort
O(N log(N)) ソート.
未完成?
**** radix-sort
O(NK) 安定ソート.
unwrapped_vector_m が必要.
内部では 0から9でのバケットソートでソートしているが, 0から2^4-1でのバケットソートにした方が除算と桁数K的に速くなるかもしれない.
*** search
あんまり使わない.
自分で書いた方が早いかも.
**** binary_search
二分探索する...
**** lower_bound
値がv 以上になる左から数えて初めての要素のインデックスを返す.
**** upper_bound
値がv より大きくなる左から数えて初めての要素のインデックスを返す.
*** math
とくになし
*** data structure
可変長配列, キュー, 優先度付きキュー, 両端キュー, BITは(たまに)よく使う.
B木は今は int32 にのみ対応, 使えるかもしれない.
**** Comparable
比較可能な型.
使っていない.
**** String
参照カウント型の文字列.
使っていないが, 文字列をソートするときに, module 内部のみで使うことを検討中.
比較可能.
**** Tuple
tuple2 と tuple3 がある.
比較可能.
***** + priority_queue
ダイクストラ法で使う.
tuple2 と tuple3 の priority_queue も Noweb で実装されている.
**** 連結リスト(linked_list)
使ってない.
**** 可変長配列(vector, unwrapped_vector)
ほぼ必須.
unwrapped_vector は メンバ変数 arr_(:) を public にしている(実行速度のため).
**** キュー(queue)
BFS で使う.
unwrapped_vector_m に依存.
**** 優先度付きキュー(priority_queue)
たまに使う.
**** 両端キュー(vec_deque)
使えそう.
可変長配列で実装してあるから, メモリアクセスの局所性が高い.
また, O(1) で k番目の要素にアクセスできる.
**** Hash Table
未完成.
open addressing hash table.
**** tree
平衡ではない二分木.
BTreeを使うべし.
**** B木(BTree)
平衡二分木.
メモリアクセスの局所性が良くなるらしい.
**** stream of output
Fortran で C++ の cout を再現したかった.
良い感じの2項演算子がない...
**** modint(未実装)
**** binary_indexed_tree(Fenwick Tree)
数列の区間和を O(logN)で求めることができる.
** コードの生成
名前は C++ や Rust を参考.
- Emacs の org-babel, tangle, Noweb を使って, 変数や Noweb のマクロを使った Fortran コードを展開します.
** [0/2] TODO
*** TODO [0/5] 色々追加する.
**** TODO [0/2] deque
***** TODO deque を色々な型へ.
***** TODO deque に iterator を実装.
**** TODO [0/1] vector
***** TODO vector に iterator を実装.
unwrapped vector を削除する?
**** TODO [0/2] BTree
***** TODO BTree を色々な型へ.
***** TODO BTree にインデックスでアクセスできるように.
**** TODO [0/2] modint
***** TODO modint を実装する.
***** TODO modint を色々なコンテナの中へ.
**** TODO [0/1] 遅延Segment Tree
***** TODO 遅延Segment Tree の 実装したい
*** TODO [0/2] 分かりやすくする.
**** TODO コードの整理をする.
- 思い付いた順番で追加してたので分類が雑.
- GFortran のコンパイルオプション -cpp は AtCoder の提出環境では付いていないので, -cpp に依存しないコードを目指す.
  エラーを module の上に伝播させて, そこでエラー処理した方が良いかもしれない.
- コードのテストを小さく分割しないとPDFの1ページにコードが収まらない.
**** TODO ドキュメントを書く.
- ドキュメントに一貫性を持たせる.
  コメントも日本語で良いかも.
  コメント要るのか検討する.
- Fortran の assumed shape array 辺りを考慮にいれていないので, それをなんとかするべきかを検討する.
