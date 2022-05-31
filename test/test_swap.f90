program test_swap
  use, intrinsic :: iso_fortran_env
  use swap_m
  implicit none
  integer      :: i
  integer      :: a(6) = [1, 2, 3, 4, 5, 6]
  integer      :: a_first(6)
  integer      :: tmp_i
  real(real64) :: b(6)
  real(real64) :: b_first(6)
  real(real64) :: tmp_r
  real(real64) :: epsilon = 1d-6
  character(len=3) :: strs(4) = [character(len=3)::"hi", "hoi", "hey", "hui"]

  a_first(:) = a(:)
  print'(*(i0, " "))', (a(i), i = 1, size(a))
  call swap(a, 2, 1)
  print'(*(i0, " "))', (a(i), i = 1, size(a))
  if (sum(a_first - a) /= a_first(2) - a(1) + a_first(1) - a(2)) then
     error stop 1
  end if

  call random_number(b)
  b_first(:) = b(:)
  print'(*(f5.3, " "))', (b(i), i = 1, size(b))
  call swap(b, 3, 4)
  print'(*(f5.3, " "))', (b(i), i = 1, size(b))
  if (abs(sum(b_first - b) - (b_first(4) - b(3) + b_first(3) - b(4))) > epsilon) then
     error stop 2
  end if
  print'(4(a, ", "))', (strs(i), i = 1, size(strs))
  call swap(strs, 4, 1)
  print'(4(a, ", "))', (strs(i), i = 1, size(strs))

end program test_swap
