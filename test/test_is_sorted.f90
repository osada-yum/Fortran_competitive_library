program is_sorted_test
  use, intrinsic :: iso_fortran_env
  use is_sorted_m
  use merge_sort_m
  implicit none
  integer(int64) :: sorted_arr(4) = [1_int64, 10_int64, 10_int64, 100_int64]
  real(real32) :: arr(10)
  call random_number(arr)
  if (.not. is_sorted(sorted_arr)) then
     error stop 1
  end if
  call merge_sort(arr)
  if (.not. is_sorted(arr)) then
     error stop 2
  end if
end program is_sorted_test
