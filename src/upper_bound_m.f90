module upper_bound_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  interface upper_bound
     module procedure :: upper_bound_int8
  end interface upper_bound
  
  interface upper_bound
     module procedure :: upper_bound_int16
  end interface upper_bound
  
  interface upper_bound
     module procedure :: upper_bound_int32
  end interface upper_bound
  
  interface upper_bound
     module procedure :: upper_bound_int64
  end interface upper_bound
  
  interface upper_bound
     module procedure :: upper_bound_real32
  end interface upper_bound
  
  interface upper_bound
     module procedure :: upper_bound_real64
  end interface upper_bound
  
  public :: upper_bound
contains
  !> upper_bound_int8: Search
  pure integer(int32) function upper_bound_int8(arr, val) result(res)
    integer(int8), intent(in) :: arr(:)
    integer(int8), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_int8
  
  !> upper_bound_int16: Search
  pure integer(int32) function upper_bound_int16(arr, val) result(res)
    integer(int16), intent(in) :: arr(:)
    integer(int16), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_int16
  
  !> upper_bound_int32: Search
  pure integer(int32) function upper_bound_int32(arr, val) result(res)
    integer(int32), intent(in) :: arr(:)
    integer(int32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_int32
  
  !> upper_bound_int64: Search
  pure integer(int32) function upper_bound_int64(arr, val) result(res)
    integer(int64), intent(in) :: arr(:)
    integer(int64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_int64
  
  !> upper_bound_real32: Search
  pure integer(int32) function upper_bound_real32(arr, val) result(res)
    real(real32), intent(in) :: arr(:)
    real(real32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_real32
  
  !> upper_bound_real64: Search
  pure integer(int32) function upper_bound_real64(arr, val) result(res)
    real(real64), intent(in) :: arr(:)
    real(real64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(p) > val) then
       res = p
       return
    else if (arr(r) <= val) then
       res = r + 1
       return
    end if
    !> a, b, ..., k, , l, ..., z
    !> arr(p) <= val
    !> arr(r) >  val
    do
       q = (p+r)/2
       if (p + 1 >= r) exit
       if (arr(q) > val) then
          r = q
       else
          p = q
       end if
    end do
    res = r
  end function upper_bound_real64
  
end module upper_bound_m
