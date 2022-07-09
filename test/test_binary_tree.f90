! [[file:../Implementations.org::binary-tree-test][binary-tree-test]]
program test_binary_tree
  use, intrinsic :: iso_fortran_env
  use is_sorted_m
  use binary_tree_m
  implicit none
  integer(int32) :: i
  type(binary_tree_int32) :: bt_int32, bt2, bt_only_l, bt_only_r
  type(binary_tree_iterator_int32) :: bt_iter
  bt_int32 = binary_tree_int32()
  bt2 = binary_tree_int32([10, 5, 6, 7, 8, 9, 15, 3, 18, 2, 4, 19, 17, 1, 0, -1, -2, 11, 16, 14, 12, 13])
  bt_only_l = binary_tree_int32([(i, i = 10, 1, -1)])
  bt_only_r = binary_tree_int32([(i, i = 1, 10)])
  call bt_int32%insert(10)
  call bt_int32%insert(-11)
  call bt_int32%insert(1)
  call bt_int32%insert(20)
  call bt_int32%insert(17)
  if (size(bt_int32) /= 5) then
     write(error_unit, *) "size error: ", size(bt_int32)
     error stop 1
  end if
  block
    integer(int32) :: sorted_arr(size(bt_only_l)), sorted_arr2(size(bt_only_l))
    call bt_iter%to(bt_only_l%begin())
    do i = 1, size(bt_only_l)
       if (bt_iter%is_null()) exit
       sorted_arr(i) = bt_iter%val()
       call bt_iter%next()
    end do
    if (.not. is_sorted(sorted_arr)) then
       write(error_unit, *) sorted_arr(:)
       error stop 2
    end if
    call bt_only_l%to_array(sorted_arr2)
    if (any(sorted_arr2 /= sorted_arr)) then
       write(error_unit, *) sorted_arr
       write(error_unit, *) sorted_arr2
       error stop 3
    end if
  end block
  block
    integer(int32) :: sorted_arr(size(bt_only_l)), sorted_arr2(size(bt_only_l))
    call bt_iter%to(bt_only_l%end())
    do i = 1, size(bt_only_l)
       if (bt_iter%is_null()) exit
       sorted_arr(i) = bt_iter%val()
       call bt_iter%pred()
    end do
    if (.not. is_sorted_descending(sorted_arr)) then
       write(error_unit, *) sorted_arr(:)
       error stop 4
    end if
    call bt_only_l%to_array_reverse(sorted_arr2)
    if (any(sorted_arr2 /= sorted_arr)) then
       write(error_unit, *) sorted_arr
       write(error_unit, *) sorted_arr2
       error stop 5
    end if
  end block
  block
    integer(int32) :: sorted_arr(size(bt_only_r)), sorted_arr2(size(bt_only_r))
    call bt_iter%to(bt_only_r%end())
    do i = 1, size(bt_only_r)
       if (bt_iter%is_null()) exit
       sorted_arr(i) = bt_iter%val()
       call bt_iter%pred()
    end do
    if (.not. is_sorted_descending(sorted_arr)) then
       write(error_unit, *) sorted_arr(:)
       error stop 6
    end if
    call bt_only_r%to_array_reverse(sorted_arr2)
    if (any(sorted_arr2 /= sorted_arr)) then
       write(error_unit, *) sorted_arr
       write(error_unit, *) sorted_arr2
       error stop 7
    end if
  end block
  block
    integer(int32) :: sorted_arr(size(bt2)), sorted_arr2(size(bt2))
    call bt_iter%to(bt2%begin())
    do i = 1, size(bt2)
       if (bt_iter%is_null()) exit
       sorted_arr(i) = bt_iter%val()
       call bt_iter%next()
    end do
    if (.not. is_sorted(sorted_arr)) then
       write(error_unit, *) sorted_arr(:)
       error stop 8
    end if
    call bt2%to_array(sorted_arr2)
    if (any(sorted_arr2 /= sorted_arr)) then
       write(error_unit, *) sorted_arr
       write(error_unit, *) sorted_arr2
       error stop 9
    end if
  end block
end program
! binary-tree-test ends here
