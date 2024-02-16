# Fortran AtCoder用ライブラリー

## 目的

AtCoder の問題を解くための fypp ソースコード.
GFortran, fypp を使うことを想定.

## 使い方

- `src/` 以下のファイル `special.fypp` を `#:include "filepath/special.fypp"` で読み込む.
- `src/` 以下のファイルを見てみる. 例えば `tuple2` を使うときは `src/tuple2_m.fypp` の先頭のコメントで, どのような fypp の変数を定義すればよいかが分かる.

- 例 (`#` で始まるものは fypp の命令.)
```sample.fypp
#:include "src/special.fypp"
#:set TUPLE2_ITEM1_TYPES = ["integer"]
#:set TUPLE2_ITEM2_TYPES = ["integer"]
#:set TUPLE2_ITEM1_KINDS = ["int32"]
#:set TUPLE2_ITEM2_KINDS = ["int32"]
#:set TUPLE2_USE_MODULES = []
#:include "src/tuple2_m.fypp"
#:set UNWRAPPED_VECTOR_ITEM_TYPES = ["type"]
#:set UNWRAPPED_VECTOR_ITEM_KINDS = ["tuple2_int32_int32"]
#:set UNWRAPPED_VECTOR_USE_MODULES = ["tuple2_m"]
#:include "src/unwrapped_vector_m.fypp"

program ${PROGNAME}$
  use, intrinsic :: iso_fortran_env
  ${USE_STATEMENT}$
  implicit none
end program ${PROGNAME}$
```
として, fypp で処理すると tuple2 の vector を生成できる.
```
$ fypp -m random -m os sample.fypp
```
`-m random` と `-m os` を失くすには, `src/special.fypp` の `PROBABILITY` 変数と `PROGNAME` 変数を書き変える.

## Implemented
- [x] 可変長配列 `src/unwrapped_vector_m.fypp`.
- [x] 可変長文字 `src/vec_string_m.fypp`.
  + `s%read(input_unit)` で任意の長さの入力を扱える.
- [x] merge sort `src/merge_sort_m.fypp`.
- [x] tuple2 `src/tuple2_m.fypp`.
- [x] tuple3 `src/tuple3_m.fypp`.
- [x] modint `src/modint_m.fypp`.
- [x] 座標圧縮 `src/coordinate_compress_m.fypp`.
  + `merge_sort_m` も必要.
- [x] union find `src/union_find_m.fypp`.
- [x] deque `src/vec_deque_m.fypp`.
- [x] 優先度付きキュー `src/priority_queue_m.fypp`.
- [x] B木 `src/btree_m.fypp`.
  + キーは比較可能でなければならない. 組込み型(integer, realなど)はキーに使える.
  + 自作の型については `src/attr_ordering.fypp` を見る.
- [x] 抽象化セグメント木 `src/segment_tree_m.fypp`.
  + `monoid_op_*` 型を継承すれば, 自作のモノイドを載せることが可能.
- [x] 抽象化遅延伝播セグメント木 `src/lazy_segment_tree_m.fypp`.
  + こちらも `monoid_*_lazy_*` 型を継承すれば, 自作のモノイドを載せることが可能.

## TODO
- [ ] Hash Map
  + B木を使えばよいので, モチベーションは低い.
