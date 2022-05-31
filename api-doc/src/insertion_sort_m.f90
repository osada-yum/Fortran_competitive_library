module insertion_sort_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  public :: insertion_sort
  interface insertion_sort
     module procedure :: insertion_sort_int32, insertion_sort_int64
     module procedure :: insertion_sort_real32, insertion_sort_real64
  end interface insertion_sort

contains

  subroutine insertion_sort_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    integer(int32)                :: key
    integer                       :: arr_size, i, j
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
  subroutine insertion_sort_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    integer(int64)                :: key
    integer                       :: arr_size, i, j
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
  subroutine insertion_sort_real32(arr)
    real(real32), intent(inout) :: arr(:)
    real(real32)                :: key
    integer                     :: arr_size, i, j
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
  subroutine insertion_sort_real64(arr)
    real(real64), intent(inout) :: arr(:)
    real(real64)                :: key
    integer                     :: arr_size, i, j
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

end module insertion_sort_m
