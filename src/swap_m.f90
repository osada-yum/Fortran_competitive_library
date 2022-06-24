module swap_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: swap
  !> ,swap: swap the two elements in the array.
  !> This is generic function for (int32, int64, real32, real64, character).
  interface swap
     module procedure :: swap_int32, swap_int64
     module procedure :: swap_real32, swap_real64
     module procedure :: swap_character
  end interface swap

contains

  subroutine swap_int32(i, j)
    integer(int32), intent(inout) :: i, j
    integer(int32) :: tmp
    tmp = i
    i   = j
    j   = tmp
  end subroutine swap_int32
  
  subroutine swap_int64(i, j)
    integer(int64), intent(inout) :: i, j
    integer(int64) :: tmp
    tmp = i
    i   = j
    j   = tmp
  end subroutine swap_int64
  
  subroutine swap_real32(i, j)
    real(real32), intent(inout) :: i, j
    real(real32) :: tmp
    tmp = i
    i   = j
    j   = tmp
  end subroutine swap_real32
  
  subroutine swap_real64(i, j)
    real(real64), intent(inout) :: i, j
    real(real64) :: tmp
    tmp = i
    i   = j
    j   = tmp
  end subroutine swap_real64
  
  subroutine swap_character(i, j)
    character(len=*), intent(inout) :: i, j
    character(len=:), allocatable :: tmp
    tmp = i
    i   = j
    j   = tmp
  end subroutine swap_character
  

end module swap_m
