program test_swap
  use, intrinsic :: iso_fortran_env
  use swap_m
  implicit none
  integer      :: i
  integer      :: a(6) = [1, 2, 3, 4, 5, 6], a_init(6)
  integer      :: tmp_i
  real(real64) :: b(6), b_first(6)
  real(real64) :: epsilon = 1d-6
  character(len=3) :: strs(4) = [character(len=3)::"hi", "hoi", "hey", "hui"], strs_init(4)

  a_init(:) = a(:)
  call swap(a(1), a(1))
  if (sum(a_init - a) /= 0) then
     error stop 1
  end if
  ! print'(*(i0, " "))', (a(i), i = 1, size(a))
  call swap(a(2), a(1))
  ! print'(*(i0, " "))', (a(i), i = 1, size(a))
  if (a_init(2) /= a(1) .or. a_init(1) /= a(2)) then
     error stop 2
  end if

  call random_number(b)
  b_first(:) = b(:)
  ! print'(*(f5.3, " "))', (b(i), i = 1, size(b))
  call swap(b(3), b(4))
  ! print'(*(f5.3, " "))', (b(i), i = 1, size(b))
  if (abs(b_first(4) - b(3)) > epsilon .or. abs(b_first(3) - b(4)) > epsilon) then
     error stop 3
  end if

  strs_init = strs
  ! print'(4(a, ", "))', (strs(i), i = 1, size(strs))
  call swap(strs(4), strs(1))
  ! print'(4(a, ", "))', (strs(i), i = 1, size(strs))
  if (strs_init(4) /= strs(1) .or. strs_init(1) /= strs(4)) then
     error stop 4
  end if

end program test_swap
