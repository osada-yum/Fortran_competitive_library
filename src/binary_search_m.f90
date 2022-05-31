module binary_search_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: binary_search
  interface binary_search
     module procedure :: binary_search_int32, binary_search_int64
  end interface binary_search

contains

  pure integer function binary_search_int32(v, arr, lb, ub) result(pos)
    integer(int32), intent(in) :: v
    integer(int32), intent(in) :: arr(lb:ub)
    integer(int32), intent(in) :: lb, ub
    integer(int32) :: p, q, r
    !> ,binary_search: Search v from arr
    !> arguments:
    !> v: typeof(v).
    !> arr: array of some type.
    !> lb, ub: integer, lower bound and upper bound of arr.
    !> return:
    !> pos: position of v in arr if lb <= pos <= ub.
    !> v does not exist in arr if pos = lb-1.
    !> variables:
    !> p, r: integer, range of search [p, r]
    !> q: integer, q = floor( (p+r)/2 ).
    p = lb
    r = ub
    do
       if (p > r) then
          pos = lb-1
          return
       end if
       q = int((p+r)/2, int32)
       if (arr(q) == v) then
          pos = q
          return
       else if (arr(q) < v) then
          p = q + 1
       else
          r = q - 1
       end if
    end do
  end function binary_search_int32
  pure integer function binary_search_int64(v, arr, lb, ub) result(pos)
    integer(int64), intent(in) :: v
    integer(int64), intent(in) :: arr(lb:ub)
    integer(int32), intent(in) :: lb, ub
    integer(int32) :: p, q, r
    !> ,binary_search: Search v from arr
    !> arguments:
    !> v: typeof(v).
    !> arr: array of some type.
    !> lb, ub: integer, lower bound and upper bound of arr.
    !> return:
    !> pos: position of v in arr if lb <= pos <= ub.
    !> v does not exist in arr if pos = lb-1.
    !> variables:
    !> p, r: integer, range of search [p, r]
    !> q: integer, q = floor( (p+r)/2 ).
    p = lb
    r = ub
    do
       if (p > r) then
          pos = lb-1
          return
       end if
       q = int((p+r)/2, int32)
       if (arr(q) == v) then
          pos = q
          return
       else if (arr(q) < v) then
          p = q + 1
       else
          r = q - 1
       end if
    end do
  end function binary_search_int64

end module binary_search_m
