module is_sorted_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: is_sorted, is_sorted_descending
  !> ,is_sorted: Check arr is sorted and return logical value.
  !> This is generic function for (int32, int64, real32, real64, character).
  interface is_sorted
     module procedure :: is_sorted_int32, is_sorted_int64
     module procedure :: is_sorted_real32, is_sorted_real64
     module procedure :: is_sorted_character
  end interface is_sorted
  interface is_sorted_descending
     module procedure :: is_sorted_descending_int32, is_sorted_descending_int64
     module procedure :: is_sorted_descending_real32, is_sorted_descending_real64
     module procedure :: is_sorted_descending_character
  end interface is_sorted_descending

contains

!!! Check an array is sorted in the ascending order.
  !> ,is_sorted: Check arr is sorted in the <= order.
  !> arguments:
  !> arr: array of integer(int32).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_int32(arr) result(sorted)
    integer(int32), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) <= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_int32
  
  !> ,is_sorted: Check arr is sorted in the <= order.
  !> arguments:
  !> arr: array of integer(int64).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_int64(arr) result(sorted)
    integer(int64), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) <= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_int64
  
  !> ,is_sorted: Check arr is sorted in the <= order.
  !> arguments:
  !> arr: array of real(real32).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_real32(arr) result(sorted)
    real(real32), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) <= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_real32
  
  !> ,is_sorted: Check arr is sorted in the <= order.
  !> arguments:
  !> arr: array of real(real64).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_real64(arr) result(sorted)
    real(real64), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) <= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_real64
  
  !> ,is_sorted: Check arr is sorted in the <= order.
  !> arguments:
  !> arr: array of character(len=*).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_character(arr) result(sorted)
    character(len=*), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) <= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_character
  
!!! Check an array is sorted in the descending order.
  !> ,is_sorted: Check arr is sorted in the >= order.
  !> arguments:
  !> arr: array of integer(int32).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_descending_int32(arr) result(sorted)
    integer(int32), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) >= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_descending_int32
  
  !> ,is_sorted: Check arr is sorted in the >= order.
  !> arguments:
  !> arr: array of integer(int64).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_descending_int64(arr) result(sorted)
    integer(int64), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) >= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_descending_int64
  
  !> ,is_sorted: Check arr is sorted in the >= order.
  !> arguments:
  !> arr: array of real(real32).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_descending_real32(arr) result(sorted)
    real(real32), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) >= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_descending_real32
  
  !> ,is_sorted: Check arr is sorted in the >= order.
  !> arguments:
  !> arr: array of real(real64).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_descending_real64(arr) result(sorted)
    real(real64), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) >= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_descending_real64
  
  !> ,is_sorted: Check arr is sorted in the >= order.
  !> arguments:
  !> arr: array of character(len=*).
  !> return:
  !> sorted: logical, .true. if arr is sorted.
  !> variables:
  !> i: integer, loop counter.
  pure logical function is_sorted_descending_character(arr) result(sorted)
    character(len=*), intent(in) :: arr(:)
    integer(int32) :: i
    sorted = .true.
    do i = 1, size(arr)-1
       if (.not. (arr(i) >= arr(i+1))) then
          sorted = .false.
          return
       end if
    end do
  end function is_sorted_descending_character
  

end module is_sorted_m
