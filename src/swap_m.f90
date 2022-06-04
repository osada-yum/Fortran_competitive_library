module swap_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: swap
  !> ,swap: swap two elements of array.
  !> This is generic function for (int32, int64, real32, real64, character).
  interface swap
     module procedure :: swap_int32, swap_int64
     module procedure :: swap_real32, swap_real64
     module procedure :: swap_character
  end interface swap

contains

  !> ,swap_int32: Swap two elements in arr.
  !> arguments:
  !> arr: array of integer(int32).
  !> i, j: integer, indices of array.
  !> variables:
  !> tmp: integer(int32), temporary variable for swap.
  subroutine swap_int32(arr, i, j)
    integer(int32), intent(inout) :: arr(:)
    integer(int32), intent(in) :: i, j
    integer(int32) :: tmp
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_int32
  
  !> ,swap_int64: Swap two elements in arr.
  !> arguments:
  !> arr: array of integer(int64).
  !> i, j: integer, indices of array.
  !> variables:
  !> tmp: integer(int64), temporary variable for swap.
  subroutine swap_int64(arr, i, j)
    integer(int64), intent(inout) :: arr(:)
    integer(int32), intent(in) :: i, j
    integer(int64) :: tmp
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_int64
  
  !> ,swap_real32: Swap two elements in arr.
  !> arguments:
  !> arr: array of real(real32).
  !> i, j: integer, indices of array.
  !> variables:
  !> tmp: real(real32), temporary variable for swap.
  subroutine swap_real32(arr, i, j)
    real(real32), intent(inout) :: arr(:)
    integer(int32), intent(in) :: i, j
    real(real32) :: tmp
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_real32
  
  !> ,swap_real64: Swap two elements in arr.
  !> arguments:
  !> arr: array of real(real64).
  !> i, j: integer, indices of array.
  !> variables:
  !> tmp: real(real64), temporary variable for swap.
  subroutine swap_real64(arr, i, j)
    real(real64), intent(inout) :: arr(:)
    integer(int32), intent(in) :: i, j
    real(real64) :: tmp
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_real64
  
  !> ,swap_character: Swap two elements in arr.
  !> arguments:
  !> arr: array of character(len=*).
  !> i, j: integer, indices of array.
  !> variables:
  !> tmp: character(len=:), allocatable, temporary variable for swap.
  subroutine swap_character(arr, i, j)
    character(len=*), intent(inout) :: arr(:)
    integer(int32), intent(in) :: i, j
    character(len=:), allocatable :: tmp
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_character
  

end module swap_m
