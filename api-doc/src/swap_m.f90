module swap_m
  use, intrinsic :: iso_fortran_env
  implicit none

  interface swap
     module procedure :: swap_int32, swap_int64
     module procedure :: swap_real32, swap_real64
  end interface swap

contains

  subroutine swap_int32(arr, i, j)
    integer(int32), intent(inout) :: arr(:)
    integer(int32), intent(in)    :: i, j
    integer(int32) :: tmp
    !> swap: Swap two elements of arr.
    !> arguments:
    !> arr: array of some type.
    !> i, j: integer indices.
    !> variables:
    !> tmp: typeof(arr).
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_int32
  subroutine swap_int64(arr, i, j)
    integer(int64), intent(inout) :: arr(:)
    integer(int32), intent(in)    :: i, j
    integer(int64) :: tmp
    !> swap: Swap two elements of arr.
    !> arguments:
    !> arr: array of some type.
    !> i, j: integer indices.
    !> variables:
    !> tmp: typeof(arr).
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_int64
  subroutine swap_real32(arr, i, j)
    real(real32)  , intent(inout) :: arr(:)
    integer(int32), intent(in)    :: i, j
    real(real32) :: tmp
    !> swap: Swap two elements of arr.
    !> arguments:
    !> arr: array of some type.
    !> i, j: integer indices.
    !> variables:
    !> tmp: typeof(arr).
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_real32
  subroutine swap_real64(arr, i, j)
    real(real64)  , intent(inout) :: arr(:)
    integer(int32), intent(in)    :: i, j
    real(real64) :: tmp
    !> swap: Swap two elements of arr.
    !> arguments:
    !> arr: array of some type.
    !> i, j: integer indices.
    !> variables:
    !> tmp: typeof(arr).
    tmp    = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  end subroutine swap_real64
end module swap_m
