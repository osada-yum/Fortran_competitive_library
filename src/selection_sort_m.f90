module selection_sort_m
  use, intrinsic :: iso_fortran_env
  use swap_m
  implicit none
  private
  public :: selection_sort
  interface selection_sort
     module procedure :: selection_sort_int32, selection_sort_int64
     module procedure :: selection_sort_real32, selection_sort_real64
  end interface selection_sort

contains

  subroutine selection_sort_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    integer :: arr_size, mini_index, i, j
    !> ,selection_sort: Sort arr of some type by selection-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> mini_index: integer, index of minimum value in arr(j:arr_size).
    !> i, j: integer, loop counters.
    arr_size = size(arr)
    do j = 1, arr_size
       mini_index = j
       do i = j+1, arr_size
          if (arr(i) < arr(mini_index)) then
             mini_index = i
          end if
       end do
       call swap(arr, j, mini_index)
    end do
  end subroutine selection_sort_int32
  subroutine selection_sort_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    integer :: arr_size, mini_index, i, j
    !> ,selection_sort: Sort arr of some type by selection-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> mini_index: integer, index of minimum value in arr(j:arr_size).
    !> i, j: integer, loop counters.
    arr_size = size(arr)
    do j = 1, arr_size
       mini_index = j
       do i = j+1, arr_size
          if (arr(i) < arr(mini_index)) then
             mini_index = i
          end if
       end do
       call swap(arr, j, mini_index)
    end do
  end subroutine selection_sort_int64
  subroutine selection_sort_real32(arr)
    real(real32), intent(inout) :: arr(:)
    integer :: arr_size, mini_index, i, j
    !> ,selection_sort: Sort arr of some type by selection-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> mini_index: integer, index of minimum value in arr(j:arr_size).
    !> i, j: integer, loop counters.
    arr_size = size(arr)
    do j = 1, arr_size
       mini_index = j
       do i = j+1, arr_size
          if (arr(i) < arr(mini_index)) then
             mini_index = i
          end if
       end do
       call swap(arr, j, mini_index)
    end do
  end subroutine selection_sort_real32
  subroutine selection_sort_real64(arr)
    real(real64), intent(inout) :: arr(:)
    integer :: arr_size, mini_index, i, j
    !> ,selection_sort: Sort arr of some type by selection-sort.
    !> arguments:
    !> arr: array of some type.
    !> variables:
    !> arr_size: integer, size of arr(:).
    !> mini_index: integer, index of minimum value in arr(j:arr_size).
    !> i, j: integer, loop counters.
    arr_size = size(arr)
    do j = 1, arr_size
       mini_index = j
       do i = j+1, arr_size
          if (arr(i) < arr(mini_index)) then
             mini_index = i
          end if
       end do
       call swap(arr, j, mini_index)
    end do
  end subroutine selection_sort_real64

end module selection_sort_m
