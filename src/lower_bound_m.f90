module lower_bound_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  interface lower_bound
     module procedure :: lower_bound_int8
  end interface lower_bound
  
  interface lower_bound
     module procedure :: lower_bound_int16
  end interface lower_bound
  
  interface lower_bound
     module procedure :: lower_bound_int32
  end interface lower_bound
  
  interface lower_bound
     module procedure :: lower_bound_real32
  end interface lower_bound
  
  interface lower_bound
     module procedure :: lower_bound_real64
  end interface lower_bound
  
  public :: lower_bound
contains
  !> lower_bound_int8: Search
  pure integer(int32) function lower_bound_int8(arr, val)
    integer(int8), intent(in) :: arr(:)
    integer(int8), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_int8 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_int8 = q
  end function lower_bound_int8
  
  !> lower_bound_int16: Search
  pure integer(int32) function lower_bound_int16(arr, val)
    integer(int16), intent(in) :: arr(:)
    integer(int16), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_int16 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_int16 = q
  end function lower_bound_int16
  
  !> lower_bound_int32: Search
  pure integer(int32) function lower_bound_int32(arr, val)
    integer(int32), intent(in) :: arr(:)
    integer(int32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_int32 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_int32 = q
  end function lower_bound_int32
  
  !> lower_bound_int64: Search
  pure integer(int32) function lower_bound_int64(arr, val)
    integer(int64), intent(in) :: arr(:)
    integer(int64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_int64 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_int64 = q
  end function lower_bound_int64
  
  !> lower_bound_real32: Search
  pure integer(int32) function lower_bound_real32(arr, val)
    real(real32), intent(in) :: arr(:)
    real(real32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_real32 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_real32 = q
  end function lower_bound_real32
  
  !> lower_bound_real64: Search
  pure integer(int32) function lower_bound_real64(arr, val)
    real(real64), intent(in) :: arr(:)
    real(real64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = size(arr)
    if (arr(r) < val) then
       lower_bound_real64 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (arr(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_real64 = q
  end function lower_bound_real64
  
end module lower_bound_m
