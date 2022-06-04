program test_insertion_sort
  use, intrinsic :: iso_fortran_env
  use is_sorted_m
  use insertion_sort_m
  implicit none
  integer      :: i
  integer      :: a(6) = [31, 41, 59, 26, 41, 58]
  real(real64) :: b(6)

  ! print'(*(i0, " "))', (a(i), i = 1, size(a))
  call insertion_sort(a)
  if (.not. is_sorted(a)) error stop 1
  ! print'(*(i0, " "))', (a(i), i = 1, size(a))
  call random_number(b)
  ! print'(*(f5.3, " "))', (b(i), i = 1, size(b))
  call insertion_sort(b)
  if (.not. is_sorted(b)) error stop 2
  ! print'(*(f5.3, " "))', (b(i), i = 1, size(b))

end program test_insertion_sort
