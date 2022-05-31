module is_sorted_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: is_sorted
  !> ,is_sorted: Check arr is sorted and return logical value.
  !> This is generic function for (int32, int64, real32, real64, character).
  interface is_sorted
     module procedure :: is_sorted_int32, is_sorted_int64
     module procedure :: is_sorted_real32, is_sorted_real64
     module procedure :: is_sorted_character
  end interface is_sorted

contains

  pure logical function is_sorted_int32(arr) result(sorted)
    integer(int32), intent(in) :: arr(:)
    !> ,is_sorted: Check arr is sorted in ascending order.
    !> arguments:
    !> arr: array of some type.
    !> return:
    !> sorted: logical, .true. if arr is sorted.
    !> variables:
    !> i, j: integer, loop counter.
    integer :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (arr(i) > arr(i+1)) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_int32
  pure logical function is_sorted_int64(arr) result(sorted)
    integer(int64), intent(in) :: arr(:)
    !> ,is_sorted: Check arr is sorted in ascending order.
    !> arguments:
    !> arr: array of some type.
    !> return:
    !> sorted: logical, .true. if arr is sorted.
    !> variables:
    !> i, j: integer, loop counter.
    integer :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (arr(i) > arr(i+1)) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_int64
  pure logical function is_sorted_real32(arr) result(sorted)
    real(real32), intent(in) :: arr(:)
    !> ,is_sorted: Check arr is sorted in ascending order.
    !> arguments:
    !> arr: array of some type.
    !> return:
    !> sorted: logical, .true. if arr is sorted.
    !> variables:
    !> i, j: integer, loop counter.
    integer :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (arr(i) > arr(i+1)) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_real32
  pure logical function is_sorted_real64(arr) result(sorted)
    real(real64), intent(in) :: arr(:)
    !> ,is_sorted: Check arr is sorted in ascending order.
    !> arguments:
    !> arr: array of some type.
    !> return:
    !> sorted: logical, .true. if arr is sorted.
    !> variables:
    !> i, j: integer, loop counter.
    integer :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (arr(i) > arr(i+1)) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_real64
  pure logical function is_sorted_character(arr) result(sorted)
    character(len=*), intent(in) :: arr(:)
    !> ,is_sorted: Check arr is sorted in ascending order.
    !> arguments:
    !> arr: array of some type.
    !> return:
    !> sorted: logical, .true. if arr is sorted.
    !> variables:
    !> i, j: integer, loop counter.
    integer :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (arr(i) > arr(i+1)) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_character

end module is_sorted_m
