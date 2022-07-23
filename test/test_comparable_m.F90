! [[file:../Implementations.org::comparable-test-module][comparable-test-module]]
module comparable_test_m
  use, intrinsic :: iso_fortran_env
  use comparable_m
  implicit none
  private
  public :: comp_int
  type, extends(comparable) :: comp_int
     integer(int32) :: val_
   contains
     procedure, pass :: val => val_comp_int
     procedure, pass :: less_equal => less_equal_comp_int
     procedure, pass :: equal => equal_comp_int
  end type comp_int
contains
  pure integer(int32) function val_comp_int(this)
    class(comp_int), intent(in) :: this
    val_comp_int = this%val_
  end function val_comp_int
  logical function less_equal_comp_int(lhs, rhs)
    class(comp_int), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    if (.not. (same_type_as(lhs, rhs))) then
       write(error_unit, '(a, i0, a)')&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') "Assertion 'same_type_as(lhs, rhs)' must be true."
       if (len_trim("in comparable_test_m:less_equal_comp_int: Do not compare different types.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'in comparable_test_m:less_equal_comp_int: Do not compare different types.'"
       end if
       error stop 1
    end if
    
    select type (rhs)
    type is (comp_int)
       less_equal_comp_int = lhs%val() <= rhs%val()
    end select
  end function less_equal_comp_int
  logical function equal_comp_int(lhs, rhs)
    class(comp_int), intent(in) :: lhs
    class(comparable), intent(in) :: rhs
    if (.not. (same_type_as(lhs, rhs))) then
       write(error_unit, '(a, i0, a)')&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') "Assertion 'same_type_as(lhs, rhs)' must be true."
       if (len_trim("in comparable_test_m:less_equal_comp_int: Do not compare different types.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'in comparable_test_m:less_equal_comp_int: Do not compare different types.'"
       end if
       error stop 2
    end if
    
    select type (rhs)
    type is (comp_int)
       equal_comp_int = lhs%val() == rhs%val()
    end select
  end function equal_comp_int
end module comparable_test_m
! comparable-test-module ends here
