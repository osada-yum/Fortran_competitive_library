module compare_m
  use, intrinsic :: iso_fortran_env
  implicit none
 private

  public :: less, greater
  public :: less_int32, less_int64, less_real32, less_real64
  public :: greater_int32, greater_int64, greater_real32, greater_real64
  public :: compare_int32, compare_int64, compare_real32, compare_real64
  interface less
     module procedure :: less_int32, less_int64, less_real32, less_real64
  end interface
  interface greater
     module procedure :: greater_int32, greater_int64, greater_real32, greater_real64
  end interface

  interface
     logical function compare_int32(x, y)
       import int32
       integer(int32), intent(in) :: x, y
     end function compare_int32
     logical function compare_int64(x, y)
       import int64
       integer(int64), intent(in) :: x, y
     end function compare_int64
     logical function compare_real32(x, y)
       import real32
       real(real32), intent(in) :: x, y
     end function compare_real32
     logical function compare_real64(x, y)
       import real64
       real(real64), intent(in) :: x, y
     end function compare_real64
  end interface

contains

  pure logical function less_int32(x, y)
    integer(int32), intent(in) :: x, y
    less_int32 = x < y
    return
  end function less_int32
  pure logical function less_int64(x, y)
    integer(int64), intent(in) :: x, y
    less_int64 = x < y
    return
  end function less_int64
  pure logical function less_real32(x, y)
    real(real32), intent(in) :: x, y
    less_real32 = x < y
    return
  end function less_real32
  pure logical function less_real64(x, y)
    real(real64), intent(in) :: x, y
    less_real64 = x < y
    return
  end function less_real64

  pure logical function greater_int32(x, y)
    integer(int32), intent(in) :: x, y
    greater_int32 = x > y
    return
  end function greater_int32
  pure logical function greater_int64(x, y)
    integer(int64), intent(in) :: x, y
    greater_int64 = x > y
    return
  end function greater_int64
  pure logical function greater_real32(x, y)
    real(real32), intent(in) :: x, y
    greater_real32 = x > y
    return
  end function greater_real32
  pure logical function greater_real64(x, y)
    real(real64), intent(in) :: x, y
    greater_real64 = x > y
    return
  end function greater_real64
end module compare_m
