program test_merge
  use, intrinsic :: iso_fortran_env
  use merge_sort_m
  implicit none
  integer(int32) :: arr(9) = [8, 3, 1, 9, 5, 4, 2, 7, 6]
  integer(int32), allocatable :: indices(:)
  integer(int32) :: i

  indices = [(i, i = 1, 9)]
  call merge_sort(arr, indices)
  if (.not. (all(arr(:) == [1,2,3,4,5,6,7,8,9]))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'all(arr(:) == [1,2,3,4,5,6,7,8,9])' must be false."
     if (len_trim("merge_sort with key is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'merge_sort with key is illegal.'"
     end if
     error stop 11
  end if
  
  if (.not. (all(indices(:) == [3,7,2,6,5,9,8,1,4]))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'all(indices(:) == [3,7,2,6,5,9,8,1,4])' must be false."
     if (len_trim("merge_sort with key is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'merge_sort with key is illegal.'"
     end if
     error stop 12
  end if
  
  call merge_sort_descending(arr)
  if (.not. (all(arr(:) == [9,8,7,6,5,4,3,2,1]))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'all(arr(:) == [9,8,7,6,5,4,3,2,1])' must be false."
     if (len_trim("merge_sort_descending is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'merge_sort_descending is illegal.'"
     end if
     error stop 21
  end if
  
  arr(1:9) = [1, 1, 1, 1, 1, 2, 2, 2, 2]
  indices(:) = [(i, i = 1, 9)]
  call merge_sort(arr, indices)
  if (.not. (all(arr(:) == [1,1,1,1,1,2,2,2,2]))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'all(arr(:) == [1,1,1,1,1,2,2,2,2])' must be false."
     if (len_trim("merge_sort with key is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'merge_sort with key is illegal.'"
     end if
     error stop 31
  end if
  
  if (.not. (all(indices(:) == [(i, i = 1, 9)]))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'all(indices(:) == [(i, i = 1, 9)])' must be false."
     if (len_trim("merge_sort with key is not stable sort.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'merge_sort with key is not stable sort.'"
     end if
     error stop 32
  end if
  
end program test_merge
