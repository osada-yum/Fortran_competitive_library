module insertion_sort_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  public :: insertion_sort, insertion_sort_descending
  !> ,insertion_sort: Sort arr in ascending order.
  !> This is generic subroutine for (int32, int64, real32, real64).
  interface insertion_sort
     module procedure :: insertion_sort_int32, insertion_sort_int64
     module procedure :: insertion_sort_real32, insertion_sort_real64
  end interface insertion_sort
  !> ,insertion_sort_descending: Sort arr in desceding order.
  !> This is generic subroutine for (int32, int64, real32, real64).
  interface insertion_sort_descending
     module procedure :: insertion_sort_descending_int32, insertion_sort_descending_int64
     module procedure :: insertion_sort_descending_real32, insertion_sort_descending_real64
  end interface insertion_sort_descending

contains

!!! Sort in ascending order.
  !> ,insertion_sort_int32: Sort arr in < order by insertion-sort.
  !> arguments:
  !> arr: Array of integer(int32).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    integer(int32)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) < key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_int32
  
  !> ,insertion_sort_int64: Sort arr in < order by insertion-sort.
  !> arguments:
  !> arr: Array of integer(int64).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    integer(int64)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) < key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_int64
  
  !> ,insertion_sort_real32: Sort arr in < order by insertion-sort.
  !> arguments:
  !> arr: Array of real(real32).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_real32(arr)
    real(real32), intent(inout) :: arr(:)
    real(real32)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) < key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_real32
  
  !> ,insertion_sort_real64: Sort arr in < order by insertion-sort.
  !> arguments:
  !> arr: Array of real(real64).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_real64(arr)
    real(real64), intent(inout) :: arr(:)
    real(real64)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) < key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_real64
  
!!! Sort in desceding order.
  !> ,insertion_sort_descending_int32: Sort arr in > order by insertion-sort.
  !> arguments:
  !> arr: Array of integer(int32).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_descending_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    integer(int32)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) > key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_descending_int32
  
  !> ,insertion_sort_descending_int64: Sort arr in > order by insertion-sort.
  !> arguments:
  !> arr: Array of integer(int64).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_descending_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    integer(int64)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) > key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_descending_int64
  
  !> ,insertion_sort_descending_real32: Sort arr in > order by insertion-sort.
  !> arguments:
  !> arr: Array of real(real32).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_descending_real32(arr)
    real(real32), intent(inout) :: arr(:)
    real(real32)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) > key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_descending_real32
  
  !> ,insertion_sort_descending_real64: Sort arr in > order by insertion-sort.
  !> arguments:
  !> arr: Array of real(real64).
  !> In end of subroutine, arr is sorted.
  !> variables:
  !> key: Same type as arr, insert key into arr(1:i-1).
  !> arr_size: integer, size of arr.
  !> i, j: integer, loop counter.
  subroutine insertion_sort_descending_real64(arr)
    real(real64), intent(inout) :: arr(:)
    real(real64)                :: key
    integer(int32) :: arr_size, i, j
    arr_size = size(arr)
    do i = 2, arr_size
       key = arr(i)
       do j = i-1, 1, -1
          if (arr(j) > key) exit
          arr(j+1) = arr(j)
       end do
       arr(j+1) = key
    end do
  end subroutine insertion_sort_descending_real64
  

end module insertion_sort_m
