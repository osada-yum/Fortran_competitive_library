# Fortran AtCoder用ライブラリー

## 目的

AtCoder の問題を解くための fypp ソースコード.
GFortran, fypp を使うことを想定.

## 使い方

- `src/` 以下のファイル `special.fypp` を `#:include "filepath/special.fypp"` で読み込む.
- `src/` 以下のファイルを見て, 例えば `tuple2` を使うときは `src/tuple2_m.fypp` を見てみる.

- 例
```
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

## TODO
- [ ] Hash Map
  + B木を使えばよいので, モチベーションは低い.
