! [[file:../Implementations.org::binary-tree-test][binary-tree-test]]
program test_binary_tree
  use, intrinsic :: iso_fortran_env
  use binary_tree_m
  implicit none
  integer :: i
  type(binary_tree_int32) :: bt_int32, bt2
  bt_int32 = binary_tree_int32()
  bt2 = binary_tree_int32([(i, i = 1,10)])
  call bt_int32%insert(10)
  write(output_unit, *) bt_int32, size(bt_int32), depth(bt_int32)
  call bt_int32%insert(1)
  write(output_unit, *) bt_int32, size(bt_int32), depth(bt_int32)
  call bt_int32%insert(17)
  write(output_unit, *) bt_int32, size(bt_int32), depth(bt_int32)
  write(output_unit, *) bt2, size(bt2), depth(bt2)
end program test_binary_tree
! binary-tree-test ends here
