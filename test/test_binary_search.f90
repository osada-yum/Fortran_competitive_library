program test_binary_search
  use, intrinsic :: iso_fortran_env
  use binary_search_m
  implicit none
  integer :: arr(-1:7) = [1, 2, 3, 4, 4, 6, 7, 8, 9]
  integer :: i
  if (binary_search(2, arr, -1, 7) /= 0) then
     error stop 1
  else if (binary_search(5, arr, -1, 7) /= lbound(arr, dim = 1)-1) then
     error stop 2
  else if (binary_search(9, arr, -1, 7) /= 7) then
     error stop 3
  end if
end program test_binary_search
