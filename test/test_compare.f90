program test_compare
  use, intrinsic :: iso_fortran_env
  use compare_m
  implicit none

  call test_comp(less_int32, 1, 3)
  if (.not. less(1, 3)) then
     error stop 1
  end if
  if (.not. greater(huge(1.0_real64), 3.0_real64)) then
     error stop 2
  end if

contains


  subroutine test_comp(cond, x, y)
    procedure(compare_int32)    :: cond
    integer(int32) , intent(in) :: x, y
    if (.not. cond(x, y)) then
       error stop 3
    end if
  end subroutine test_comp

end program test_compare
