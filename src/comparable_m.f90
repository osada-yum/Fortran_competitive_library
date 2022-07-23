! [[file:../Implementations.org::comparable-module][comparable-module]]
module comparable_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: comparable
  type :: comparable
   contains
     procedure, pass :: less    => less_comparable
     procedure, pass :: less_equal    => less_equal_comparable
     procedure, pass :: greater => greater_comparable
     procedure, pass :: greater_equal => greater_equal_comparable
     procedure, pass :: equal => equal_comparable
     procedure, pass :: not_equal => not_equal_comparable
     generic :: operator(<) => less
     generic :: operator(<=) => less_equal
     generic :: operator(>) => greater
     generic :: operator(>=) => greater_equal
     generic :: operator(==) => equal
     generic :: operator(/=) => not_equal
  end type comparable
contains
  logical function less_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    less_comparable = (lhs <= rhs) .and. (lhs /= rhs)
  end function less_comparable
  logical function less_equal_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    error stop "You must implement less_equal in the child class of comparable."
  end function less_equal_comparable
  logical function greater_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    greater_comparable = .not. (lhs <= rhs)
  end function greater_comparable
  logical function greater_equal_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    greater_equal_comparable = (lhs == rhs) .or. (.not. (lhs <= rhs))
  end function greater_equal_comparable
  logical function equal_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    equal_comparable = .not. (lhs /= rhs)
  end function equal_comparable
  logical function not_equal_comparable(lhs, rhs)
    class(comparable), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    not_equal_comparable = .not. (lhs == rhs)
  end function not_equal_comparable
end module comparable_m
! comparable-module ends here
