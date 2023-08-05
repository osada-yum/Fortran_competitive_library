module tuple3_m
  use, intrinsic :: iso_fortran_env
  implicit none
  public :: tuple3_int32_int32_int32
  type :: tuple3_int32_int32_int32
     private
     integer(int32) :: v1_
     integer(int32) :: v2_
     integer(int32) :: v3_
  end type tuple3_int32_int32_int32
  public :: construct_tuple3_int32_int32_int32
  interface tuple3_int32_int32_int32
     module procedure :: construct_tuple3_int32_int32_int32
  end interface tuple3_int32_int32_int32
  interface operator(<)
     module procedure :: less_tuple3_int32_int32_int32
  end interface operator(<)
  interface operator(<=)
     module procedure :: less_equal_tuple3_int32_int32_int32
  end interface operator(<=)
  interface operator(>)
     module procedure :: greater_tuple3_int32_int32_int32
  end interface operator(>)
  interface operator(>=)
     module procedure :: greater_equal_tuple3_int32_int32_int32
  end interface operator(>=)
  interface operator(==)
     module procedure :: equal_tuple3_int32_int32_int32
  end interface operator(==)
  interface operator(/=)
     module procedure :: not_equal_tuple3_int32_int32_int32
  end interface operator(/=)
  
  public :: tuple3_int64_int64_int64
  type :: tuple3_int64_int64_int64
     private
     integer(int64) :: v1_
     integer(int64) :: v2_
     integer(int64) :: v3_
  end type tuple3_int64_int64_int64
  public :: construct_tuple3_int64_int64_int64
  interface tuple3_int64_int64_int64
     module procedure :: construct_tuple3_int64_int64_int64
  end interface tuple3_int64_int64_int64
  interface operator(<)
     module procedure :: less_tuple3_int64_int64_int64
  end interface operator(<)
  interface operator(<=)
     module procedure :: less_equal_tuple3_int64_int64_int64
  end interface operator(<=)
  interface operator(>)
     module procedure :: greater_tuple3_int64_int64_int64
  end interface operator(>)
  interface operator(>=)
     module procedure :: greater_equal_tuple3_int64_int64_int64
  end interface operator(>=)
  interface operator(==)
     module procedure :: equal_tuple3_int64_int64_int64
  end interface operator(==)
  interface operator(/=)
     module procedure :: not_equal_tuple3_int64_int64_int64
  end interface operator(/=)
  
contains
  !> construct_tuple3_int32_int32_int32_by_size: Construct tuple3_int32_int32_int32.
  impure function construct_tuple3_int32_int32_int32(val1, val2, val3) result(res)
    type(tuple3_int32_int32_int32) :: res
    integer(int32), intent(in) :: val1
    integer(int32), intent(in) :: val2
    integer(int32), intent(in) :: val3
    res%v1_ = val1
    res%v2_ = val2
    res%v3_ = val3
  end function construct_tuple3_int32_int32_int32
  !> less_tuple3_int32_int32_int32: Compare the first elements.
  !> Compare the second elements if the first elements are same.
  logical function less_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ < rhs%v3_
       end if
    end if
  end function less_tuple3_int32_int32_int32
  logical function less_equal_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ <= rhs%v3_
       end if
    end if
  end function less_equal_tuple3_int32_int32_int32
  logical function greater_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ > rhs%v3_
       end if
    end if
  end function greater_tuple3_int32_int32_int32
  logical function greater_equal_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ >= rhs%v3_
       end if
    end if
  end function greater_equal_tuple3_int32_int32_int32
  logical function equal_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ == rhs%v1_ .and. lhs%v2_ == rhs%v2_ .and. lhs%v3_ == rhs%v3_
  end function equal_tuple3_int32_int32_int32
  logical function not_equal_tuple3_int32_int32_int32(lhs, rhs) result(res)
    type(tuple3_int32_int32_int32), intent(in) :: lhs, rhs
    res = lhs%v1_ /= rhs%v1_ .or. lhs%v2_ /= rhs%v2_ .or. lhs%v3_ /= rhs%v3_
  end function not_equal_tuple3_int32_int32_int32
  
  !> construct_tuple3_int64_int64_int64_by_size: Construct tuple3_int64_int64_int64.
  impure function construct_tuple3_int64_int64_int64(val1, val2, val3) result(res)
    type(tuple3_int64_int64_int64) :: res
    integer(int64), intent(in) :: val1
    integer(int64), intent(in) :: val2
    integer(int64), intent(in) :: val3
    res%v1_ = val1
    res%v2_ = val2
    res%v3_ = val3
  end function construct_tuple3_int64_int64_int64
  !> less_tuple3_int64_int64_int64: Compare the first elements.
  !> Compare the second elements if the first elements are same.
  logical function less_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ < rhs%v3_
       end if
    end if
  end function less_tuple3_int64_int64_int64
  logical function less_equal_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ < rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ < rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ <= rhs%v3_
       end if
    end if
  end function less_equal_tuple3_int64_int64_int64
  logical function greater_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ > rhs%v3_
       end if
    end if
  end function greater_tuple3_int64_int64_int64
  logical function greater_equal_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ > rhs%v1_
    if (lhs%v1_ == rhs%v1_) then
       res = lhs%v2_ > rhs%v2_
       if (lhs%v2_ == rhs%v2_) then
          res = lhs%v3_ >= rhs%v3_
       end if
    end if
  end function greater_equal_tuple3_int64_int64_int64
  logical function equal_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ == rhs%v1_ .and. lhs%v2_ == rhs%v2_ .and. lhs%v3_ == rhs%v3_
  end function equal_tuple3_int64_int64_int64
  logical function not_equal_tuple3_int64_int64_int64(lhs, rhs) result(res)
    type(tuple3_int64_int64_int64), intent(in) :: lhs, rhs
    res = lhs%v1_ /= rhs%v1_ .or. lhs%v2_ /= rhs%v2_ .or. lhs%v3_ /= rhs%v3_
  end function not_equal_tuple3_int64_int64_int64
  
end module tuple3_m
