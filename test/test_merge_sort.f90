program test_merge
  use, intrinsic :: iso_fortran_env
  use merge_sort_m
  implicit none
  integer :: arr(9) = [8, 3, 1, 9, 5, 4, 2, 7, 6]
  integer :: i

  print'(*(i0, " "))', (arr(i), i = 1, size(arr))
  call merge_sort(arr)
  print'(*(i0, " "))', (arr(i), i = 1, size(arr))

end program test_merge
