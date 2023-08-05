module tuple2_m
  use, intrinsic :: iso_fortran_env
  implicit none
  public :: tuple2_int32_int32
  type :: tuple2_int32_int32
     integer(int32) :: v1_
     integer(int32) :: v2_
  end type tuple2_int32_int32
  private :: construct_tuple2_int32_int32
  interface tuple2_int32_int32
     module procedure :: construct_tuple2_int32_int32
  end interface tuple2_int32_int32
  interface operator(<)
     module procedure :: less_tuple2_int32_int32
  end interface operator(<)
  interface operator(<=)
     module procedure :: less_equal_tuple2_int32_int32
  end interface operator(<=)
  interface operator(>)
     module procedure :: greater_tuple2_int32_int32
  end interface operator(>)
  interface operator(>=)
     module procedure :: greater_equal_tuple2_int32_int32
  end interface operator(>=)
  interface operator(==)
     module procedure :: equal_tuple2_int32_int32
  end interface operator(==)
  interface operator(/=)
     module procedure :: not_equal_tuple2_int32_int32
  end interface operator(/=)
  
  public :: tuple2_int64_int64
  type :: tuple2_int64_int64
     integer(int64) :: v1_
     integer(int64) :: v2_
  end type tuple2_int64_int64
  private :: construct_tuple2_int64_int64
  interface tuple2_int64_int64
     module procedure :: construct_tuple2_int64_int64
  end interface tuple2_int64_int64
  interface operator(<)
     module procedure :: less_tuple2_int64_int64
  end interface operator(<)
  interface operator(<=)
     module procedure :: less_equal_tuple2_int64_int64
  end interface operator(<=)
  interface operator(>)
     module procedure :: greater_tuple2_int64_int64
  end interface operator(>)
  interface operator(>=)
     module procedure :: greater_equal_tuple2_int64_int64
  end interface operator(>=)
  interface operator(==)
     module procedure :: equal_tuple2_int64_int64
  end interface operator(==)
  interface operator(/=)
     module procedure :: not_equal_tuple2_int64_int64
  end interface operator(/=)
  
contains
  !> construct_tuple2_int32_int32_by_size: Construct tuple2_int32_int32.
  impure function construct_tuple2_int32_int32(val1, val2) result(res)
    type(tuple2_int32_int32) :: res
    integer(int32), intent(in) :: val1
    integer(int32), intent(in) :: val2
    res%v1_ = val1
    res%v2_ = val2
  end function construct_tuple2_int32_int32
  !> less_tuple2_int32_int32: Compare the first elements.
  !> Compare the second elements if the first elements are same.
  pure logical function less_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
    end if
  end function less_tuple2_int32_int32
  pure logical function less_equal_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ <= rhs%v2_
    end if
  end function less_equal_tuple2_int32_int32
  pure logical function greater_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
    end if
  end function greater_tuple2_int32_int32
  pure logical function greater_equal_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ >= rhs%v2_
    end if
  end function greater_equal_tuple2_int32_int32
  pure logical function equal_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ == rhs%v1_ .and. lhs%v2_ == rhs%v2_
  end function equal_tuple2_int32_int32
  pure logical function not_equal_tuple2_int32_int32(lhs, rhs) result(res)
    type(tuple2_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ /= rhs%v1_ .or. lhs%v2_ /= rhs%v2_
  end function not_equal_tuple2_int32_int32
  
  !> construct_tuple2_int64_int64_by_size: Construct tuple2_int64_int64.
  impure function construct_tuple2_int64_int64(val1, val2) result(res)
    type(tuple2_int64_int64) :: res
    integer(int64), intent(in) :: val1
    integer(int64), intent(in) :: val2
    res%v1_ = val1
    res%v2_ = val2
  end function construct_tuple2_int64_int64
  !> less_tuple2_int64_int64: Compare the first elements.
  !> Compare the second elements if the first elements are same.
  pure logical function less_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
    end if
  end function less_tuple2_int64_int64
  pure logical function less_equal_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ <= rhs%v2_
    end if
  end function less_equal_tuple2_int64_int64
  pure logical function greater_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
    end if
  end function greater_tuple2_int64_int64
  pure logical function greater_equal_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ >= rhs%v2_
    end if
  end function greater_equal_tuple2_int64_int64
  pure logical function equal_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ == rhs%v1_ .and. lhs%v2_ == rhs%v2_
  end function equal_tuple2_int64_int64
  pure logical function not_equal_tuple2_int64_int64(lhs, rhs) result(res)
    type(tuple2_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ /= rhs%v1_ .or. lhs%v2_ /= rhs%v2_
  end function not_equal_tuple2_int64_int64
  
end module tuple2_m
