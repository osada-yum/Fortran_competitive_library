module bubble_sort_m
  use, intrinsic :: iso_fortran_env
  use swap_m
  implicit none
  private
  public :: bubble_sort
  interface bubble_sort
     module procedure :: bubble_sort_int32, bubble_sort_int64
     module procedure :: bubble_sort_real32, bubble_sort_real64
  end interface bubble_sort

contains

  subroutine bubble_sort_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    integer(int32) :: size_arr, i, j
    !> bubble_sort: Sort arr of some type by bubble-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> i, j: integer, loop counters.
    size_arr = size(arr)
    do i = 1, size_arr
       do j = size_arr, i+1, -1
          if (arr(j) < arr(j-1)) then
             call swap(arr, j, j-1)
          end if
       end do
    end do
  end subroutine bubble_sort_int32
  subroutine bubble_sort_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    integer(int32) :: size_arr, i, j
    !> bubble_sort: Sort arr of some type by bubble-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> i, j: integer, loop counters.
    size_arr = size(arr)
    do i = 1, size_arr
       do j = size_arr, i+1, -1
          if (arr(j) < arr(j-1)) then
             call swap(arr, j, j-1)
          end if
       end do
    end do
  end subroutine bubble_sort_int64
  subroutine bubble_sort_real32(arr)
    real(real32), intent(inout) :: arr(:)
    integer(int32) :: size_arr, i, j
    !> bubble_sort: Sort arr of some type by bubble-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> i, j: integer, loop counters.
    size_arr = size(arr)
    do i = 1, size_arr
       do j = size_arr, i+1, -1
          if (arr(j) < arr(j-1)) then
             call swap(arr, j, j-1)
          end if
       end do
    end do
  end subroutine bubble_sort_real32
  subroutine bubble_sort_real64(arr)
    real(real64), intent(inout) :: arr(:)
    integer(int32) :: size_arr, i, j
    !> bubble_sort: Sort arr of some type by bubble-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> i, j: integer, loop counters.
    size_arr = size(arr)
    do i = 1, size_arr
       do j = size_arr, i+1, -1
          if (arr(j) < arr(j-1)) then
             call swap(arr, j, j-1)
          end if
       end do
    end do
  end subroutine bubble_sort_real64

end module bubble_sort_m
